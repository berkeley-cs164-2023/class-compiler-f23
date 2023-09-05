type register = Rax | Rcx

let string_of_register = function Rax -> "rax" | Rcx -> "rcx"

type operand = Reg of register | Imm of int

let is_register o = match o with Reg _ -> true | _ -> false

let string_of_operand = function
  | Reg r -> string_of_register r
  | Imm i -> string_of_int i

type directive =
  | Global of string
  | Label of string
  | Mov of (operand * operand)
  | Add of (operand * operand)
  | Sub of (operand * operand)
  | Ret
  | Comment of string

let run cmd args =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (run cmd args |- read_all)

let macos = run "uname" [ "-s" ] |> String.trim |> String.equal "Darwin"

let string_of_directive = function
  (* frontmatter *)
  | Global l -> Printf.sprintf (if macos then "global _%s" else "global %s") l
  (* labels *)
  | Label l -> Printf.sprintf (if macos then "_%s:" else "%s:") l
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
  | Ret -> "\tret"
  | Comment s -> Printf.sprintf "; %s" s
