(* $Id: ThoUnix.ml 2499 2010-05-10 17:21:36Z ohl $ *)

open Unix

let redirect_file file open_flags perm fds =
  let file_fd = openfile file open_flags perm in
  List.iter (dup2 file_fd) fds;
  close file_fd

let redirect_files
    ?(append = false) ?(no_clobber = false) ?(perm = 0o666)
    ?in_file ?out_file ?err_file () =
  let in_flags =
    [O_RDONLY]
  and out_flags =
    [O_WRONLY; O_CREAT] @
    (if append then [O_APPEND] else [O_TRUNC]) @
    (if no_clobber then [O_EXCL] else []) in
  begin match in_file with
  | None -> ()
  | Some name -> redirect_file name in_flags 0 [stdin]
  end;
  begin match out_file, err_file with
  | None, None -> ()
  | Some name, None -> redirect_file name out_flags perm [stdout]
  | None, Some name -> redirect_file name out_flags perm [stderr]
  | Some out_name, Some err_name ->
      if out_name = err_name then
	redirect_file out_name out_flags perm [stdout; stderr]
      else
	redirect_file out_name out_flags perm [stdout];
	redirect_file err_name out_flags perm [stderr]
  end

let exec ?(search = false) ?env argv_list =
  let argv = Array.of_list argv_list in
  match env with
  | None ->
      (if search then execvp else execv) argv.(0) argv
  | Some environment ->
      (if search then execvpe else execve) argv.(0) argv (Array.of_list environment)

let run_command ?append ?no_clobber ?perm ?in_file ?out_file ?err_file ?search ?env argv =
  match argv with
  | [] -> invalid_arg "run_command: empty"
  | _ ->
      begin match fork () with
      | 0 ->
	  redirect_files ?append ?no_clobber ?perm ?in_file ?out_file ?err_file ();
	  begin try exec ?search ?env argv with _ -> exit 127 end
      | pid ->
	  let pid', status = waitpid [] pid in
	  status
      end 

let run_command_silently ?search ?env argv =
  let dev_null = "/dev/null" in
  run_command ~in_file:dev_null ~out_file:dev_null ~err_file:dev_null ?search ?env argv

let popen_in ?search ?env argv =
  let in_read, in_write = pipe () in
  let ic = in_channel_of_descr in_read in
  set_close_on_exec in_read;
  match fork() with
  | 0 ->
      dup2 in_write stdout;
      close in_write;
      begin try exec ?search ?env argv with _ -> exit 127 end
  | pid ->
      close in_write;
      (pid, ic)

let rec reap_child pid =
  try
    waitpid [] pid 
  with
  | Unix_error (EINTR, _, _) -> reap_child pid


(*
let _ =
  run_command
    ~in_file:"/dev/null" ~out_file:"foo" ~err_file:"foo"
    ~search:true ["date"; "-u"]
*)

let input_lines ic =
  let rev_lines = ref [] in
  try
    while true do
      rev_lines := input_line ic :: !rev_lines
    done;
    []
  with
  | End_of_file -> List.rev !rev_lines

let popen_lines ?search ?env argv =
  let pid, ic = popen_in ?search ?env argv in
  let lines = input_lines ic in
  close_in ic;
  match reap_child pid with
  | _, WEXITED 0 ->
      lines
  | _, WEXITED error ->
      failwith (String.concat " " argv ^ " exited w/code " ^ string_of_int error)
  | _, WSIGNALED signal ->
      failwith (String.concat " " argv ^ " caught signal #" ^ string_of_int signal)
  | _, WSTOPPED signal ->
      failwith (String.concat " " argv ^ " stopped by signal #" ^ string_of_int signal)

(*
let _ =
  let pid, ic = popen_in ~search:true ["date"; "-u"] in
  let lines = input_lines ic in
  close_in ic;
  List.iter (Printf.printf "> %s\n") lines;
  match reap_child pid with
  | pid, WEXITED rc -> Printf.printf "exited w/%d\n" rc
  | pid, WSIGNALED signal -> Printf.printf "caught signal #%d\n" signal
  | pid, WSTOPPED signal -> Printf.printf "stopped by signal #%d\n" signal
*)

let mkdir_p path = run_command_silently ["/bin/mkdir"; "-p"; path]
let rm_fr path = run_command_silently ["/bin/rm"; "-rf"; path]
let chmod_wr path = run_command_silently ["/bin/chmod"; "-R"; "u+w"; path]

let really_rm_fr path =
  let status = rm_fr path in
  match status with
  | WEXITED 0 -> status
  | _ ->
      let status = chmod_wr path in
      match status with
      | WEXITED 0 -> rm_fr path
      | _ -> status

let tm_to_string tm =
  Printf.sprintf "%04d-%02d-%02d_%02d.%02d.%02d"
    (tm.tm_year + 1900) tm.tm_mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
