open S_exp
open Asm
open Interp
open Util

exception BadExpression of s_exp

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00

let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111

let heap_mask = 0b111
let pair_tag = 0b010

let operand_of_bool (b: bool) : operand =
    Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let operand_of_num (x: int) : operand =
    Imm ((x lsl num_shift) lor num_tag)

let zf_to_bool : directive list =
        [Mov (Reg Rax, Imm 0)
        ; Setz (Reg Rax)
        ; Shl (Reg Rax, Imm bool_shift)
        ; Or (Reg Rax, Imm bool_tag)]

let lf_to_bool : directive list =
    [ Mov (Reg Rax, Imm 0)
        ; Setl (Reg Rax)
        ; Shl (Reg Rax, Imm bool_shift)
        ; Or (Reg Rax, Imm bool_tag)
        ]

let ensure_num (op : operand) : directive list =
    [   
        Mov (Reg R8, op)
        ;And (Reg R8, Imm num_mask)
        ; Cmp (Reg R8, Imm num_tag)
        ; Jnz "error"
    ]

let ensure_pair (op : operand) : directive list =
    [   
        Mov (Reg R8, op)
        ;And (Reg R8, Imm heap_mask)
        ; Cmp (Reg R8, Imm pair_tag)
        ; Jnz "error"
    ]

let stack_address (stack_index : int) = MemOffset (Reg Rsp, Imm stack_index)

let align_stack_index (stack_index : int) : int =
    if stack_index mod 16 = -8 then stack_index else stack_index - 8

let rec compile_exp (tab : int symtab) (stack_index: int) (program: s_exp): directive list =
    match program with
    | Num n ->
        [Mov (Reg Rax, operand_of_num n)]
    | Sym "true" -> [Mov (Reg Rax, operand_of_bool true)]
    | Sym "false" -> [Mov (Reg Rax, operand_of_bool false)]
    | Lst [Sym "pair"; e1; e2] ->
        compile_exp tab stack_index e1 
        @ [Mov (stack_address stack_index, Reg Rax)]
        @ compile_exp tab (stack_index - 8) e2
        @ [Mov (Reg R8, stack_address stack_index)
        ; Mov (MemOffset (Reg Rdi, Imm 0), Reg R8)
        ; Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax)
        ; Mov (Reg Rax, Reg Rdi)
        ; Or (Reg Rax, Imm pair_tag)
        ; Add (Reg Rdi, Imm 16)
        ]
    | Lst [Sym "read-num"] ->
        [
            Mov (stack_address stack_index, Reg Rdi);
            Add (Reg Rsp, Imm (align_stack_index stack_index));
            Call "read_num";
            Sub (Reg Rsp, Imm (align_stack_index stack_index));
            Mov (Reg Rdi, stack_address stack_index);
            ]
    | Lst [Sym "left"; e] ->
        compile_exp tab stack_index e 
        @ ensure_pair (Reg Rax) 
        @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag)))]
    | Lst [Sym "right"; e] ->
        compile_exp tab stack_index e 
        @ ensure_pair (Reg Rax) 
        @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8)))]
    | Lst [Sym "let"; Lst [Lst [Sym var; e]]; body] ->
        compile_exp tab stack_index e
        @ [Mov (stack_address stack_index, Reg Rax)]
        @ compile_exp (Symtab.add var stack_index tab) (stack_index - 8) body
    | Sym var when Symtab.mem var tab ->
        [Mov (Reg Rax, stack_address (Symtab.find var tab))]
    | Lst [Sym "not"; arg] ->
        compile_exp tab stack_index arg @
        [Cmp (Reg Rax, operand_of_bool false)]
        @ zf_to_bool
    | Lst [Sym "zero?"; arg] ->
        compile_exp tab stack_index arg @
        [Cmp (Reg Rax, operand_of_num 0)]
        @ zf_to_bool
    | Lst [Sym "num?"; arg] ->
        compile_exp tab stack_index arg @
        [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)]
        @ zf_to_bool
    | Lst [Sym "add1"; arg] ->
        compile_exp tab stack_index arg @
        ensure_num (Reg Rax) @
        [Add (Reg Rax, operand_of_num 1)]
    | Lst [Sym "sub1"; arg] ->
        compile_exp tab stack_index arg @
        ensure_num (Reg Rax) @
        [Sub (Reg Rax, operand_of_num 1)]
    | Lst [Sym "if"; test_exp; then_exp; else_exp] ->
        let else_label = Util.gensym "else" in 
        let continue_label = Util.gensym "continue" in
        compile_exp tab stack_index test_exp
        @ [Cmp (Reg Rax, operand_of_bool false); Jz else_label]
        @ compile_exp tab stack_index then_exp
        @ [Jmp continue_label]
        @ [Label else_label]
        @ compile_exp tab stack_index else_exp
        @ [Label continue_label]
    | Lst [Sym "+"; e1; e2] -> (
        compile_exp tab stack_index e1 
        @ ensure_num (Reg Rax)
        @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
        @ compile_exp tab (stack_index - 8) e2
        @ ensure_num (Reg Rax)
        @ [Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index))]
        @ [Add (Reg Rax, Reg R8)]
    )
    | Lst [Sym "-"; e1; e2] -> (
        compile_exp tab stack_index e1 
        @ ensure_num (Reg Rax)
        @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
        @ compile_exp tab (stack_index - 8) e2
        @ ensure_num (Reg Rax)
        @ [Mov (Reg R8, Reg Rax)]
        @ [Mov (Reg Rax, MemOffset (Reg Rsp, Imm stack_index))]
        @ [Sub (Reg Rax, Reg R8)]
    )
    | Lst [Sym "="; e1; e2] -> (
        compile_exp tab stack_index e1 
        @ ensure_num (Reg Rax)
        @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
        @ compile_exp tab (stack_index - 8) e2
        @ ensure_num (Reg Rax)
        @ [Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index))]
        @ [Cmp (Reg Rax, Reg R8)]
        @ zf_to_bool
    )
    | Lst [Sym "<"; e1; e2] -> (
        compile_exp tab stack_index e1 
        @ ensure_num (Reg Rax)
        @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
        @ compile_exp tab (stack_index - 8) e2
        @ ensure_num (Reg Rax)
        @ [Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index))]
        @ [Cmp (Reg R8, Reg Rax)]
        @ lf_to_bool
    )
    | e -> raise (BadExpression e)

let compile (program:s_exp): string =
    [Global "entry"; Extern "error"; Extern "read_num"; Label "entry"] 
    @ compile_exp Symtab.empty (-8) program
    @ [Ret]
    |> List.map string_of_directive |> String.concat "\n"

let compile_to_file (program: string): unit =
    let file = open_out "program.s" in 
    output_string file (compile (parse program));
    close_out file

let compile_and_run (program: string): string =
    compile_to_file program;
    ignore (Unix.system "nasm program.s -f macho64 -o program.o");
    ignore (Unix.system "gcc program.o runtime.o -o program");
    let inp = Unix.open_process_in "./program" in
    let r = input_line inp in
    close_in inp; r

let compile_and_run_io (program : string) (input : string) : string =
    compile_to_file program ;
    ignore (Unix.system "nasm program.s -f macho64 -o program.o") ;
    ignore (Unix.system "gcc program.o runtime.c -o program") ;
    let inp, outp = Unix.open_process "./program" in
    output_string outp input ;
    close_out outp ;
    let r = input_all inp in
    close_in inp ; r

let compile_and_run_err (program : string) : string =
    try compile_and_run program with BadExpression _ -> "ERROR"

let difftest (examples : string list) =
    let results = List.map (fun ex -> (compile_and_run_err ex, interp_err ex)) examples in 
    List.for_all (fun (r1, r2) -> r1 = r2) results

let test () =
    difftest [
        "43"; 
        "(sub1 45)"; 
        "(add1 45)";
        "(add1 false)";
        "(add1 (add1 40))";
        "(not 3)";
        "(not (not false))";
        "(not (zero? 4))";
        "(num? (add1 3))";
        "(+ 1 3)";
        "(+ false true)";
        "(add1 false)";
        "(sub1 false)";
        "(= (pair 1 2) (pair 1 2))";
        "(= 3 3)"
        ]