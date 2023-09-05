let compile (program: string): string =
    String.concat "\n" ["global _entry";
    "_entry:";
    Printf.sprintf "		mov rax, %s" program;
    "		ret"]

let compile_to_file (program: string): unit =
    let file = open_out "program.s" in 
    output_string file (compile program);
    close_out file

let compile_and_run (program: string): string =
    compile_to_file program;
    ignore (Unix.system "nasm program.s -f macho64 -o program.o");
    ignore (Unix.system "gcc program.o runtime.o -o program");
    let inp = Unix.open_process_in "./program" in
    let r = input_line inp in
    close_in inp; r