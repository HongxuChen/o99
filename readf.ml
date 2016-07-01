open Batteries;;

let filelines = File.lines_of "readf.ml"   in
    Enum.iter (fun line -> Printf.printf "%s\n" line) filelines;;
