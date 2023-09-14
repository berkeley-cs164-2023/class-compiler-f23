open S_exp
open Asm
open Interp

exception BadExpression of s_exp

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00

let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111

let rec compile_exp (program: s_exp): directive list =
    match program with
    | Num n ->
        [Mov (Reg Rax, Imm (n lsl num_shift))]
    | Sym "true" -> [Mov (Reg Rax, Imm ((1 lsl bool_shift) lor bool_tag))]
    | Sym "false" -> [Mov (Reg Rax, Imm ((0 lsl bool_shift) lor bool_tag))]
    | Lst [Sym "add1"; arg] ->
        compile_exp arg @
        [Add (Reg Rax, Imm (1 lsl num_shift))]
    | Lst [Sym "sub1"; arg] ->
        compile_exp arg @
        [Sub (Reg Rax, Imm (1 lsl num_shift))]
    | e -> raise (BadExpression e)

let compile (program: s_exp): string =
    [Global "entry"; Label "entry"] 
    @ compile_exp program
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

let difftest (examples : string list) =
    let results = List.map (fun ex -> (compile_and_run ex, interp ex)) examples in 
    List.for_all (fun (r1, r2) -> r1 = r2) results

let test () =
    difftest ["43"; "(sub1 45)"; "(add1 45)"]