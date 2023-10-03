type register = Rax | Rcx | R8 | Rsp | Rbp |Rdi

let string_of_register ?(last_byte = false) (reg : register) : string =
  match (reg, last_byte) with
  | Rax, false ->
      "rax"
  | Rax, true ->
      "al"
  | Rcx, false ->
      "rcx"
  | Rcx, true ->
      "cl"
  | R8, false ->
      "r8"
  | R8, true ->
      "r8b"
  | Rsp, true ->
      "rsp"
  | Rsp, false ->
      "rsp"
  | Rbp, true ->
      "rsp"
  | Rbp, false ->
      "rsp"
  | Rdi, true ->
      "rdi"
  | Rdi, false ->
      "rdi"

type operand = Reg of register | Imm of int | MemOffset of (operand * operand)

let is_register o = match o with Reg _ -> true | _ -> false

let rec string_of_operand ?(last_byte = false) = function
  | Reg r ->
      string_of_register ~last_byte r
  | Imm i ->
      string_of_int i
  | MemOffset (o1, o2) ->
      Printf.sprintf "[%s + %s]" (string_of_operand o1) (string_of_operand o2)

type directive =
  | Global of string
  | Extern of string
  | Label of string
  | Mov of (operand * operand)
  | Add of (operand * operand)
  | Sub of (operand * operand)
  | And of (operand * operand)
  | Or of (operand * operand)
  | Shl of (operand * operand)
  | Shr of (operand * operand)
  | Cmp of (operand * operand)
  | Setz of operand
  | Setl of operand
  | Jmp of string
  | Jz of string
  | Jnz of string
  | Call of string
  | Ret
  | Comment of string

let run cmd args =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (run cmd args |- read_all)

let macos = run "uname" ["-s"] |> String.trim |> String.equal "Darwin"

let label_name macos name = if macos then "_" ^ name else name

let string_of_directive = function
  (* frontmatter *)
  | Global l ->
      Printf.sprintf "global %s" (label_name macos l)
  | Extern l ->
      Printf.sprintf "extern %s" (label_name macos l)
  (* labels *)
  | Label l ->
      label_name macos l ^ ":"
  (* actual instructions *)
  | Mov (dest, src) ->
      Printf.sprintf "\tmov %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Add (dest, src) ->
      Printf.sprintf "\tadd %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Sub (dest, src) ->
      Printf.sprintf "\tsub %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | And (dest, src) ->
      Printf.sprintf "\tand %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Or (dest, src) ->
      Printf.sprintf "\tor %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Shl (dest, src) ->
      Printf.sprintf "\tshl %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Shr (dest, src) ->
      Printf.sprintf "\tshr %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Cmp (dest, src) ->
      Printf.sprintf "\tcmp %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Setz dest ->
      Printf.sprintf "\tsetz %s" (string_of_operand ~last_byte:true dest)
  | Setl dest ->
      Printf.sprintf "\tsetl %s" (string_of_operand ~last_byte:true dest)
  | Jmp name ->
      Printf.sprintf "\tjmp %s" (label_name macos name)
  | Jz name ->
      Printf.sprintf "\tjz %s" (label_name macos name)
  | Jnz name ->
      Printf.sprintf "\tjnz %s" (label_name macos name)
  | Call name ->
      Printf.sprintf "\tcall %s" (label_name macos name)
  | Ret ->
      "\tret"
  | Comment s ->
      Printf.sprintf "; %s" s