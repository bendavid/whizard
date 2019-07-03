(* $Id:$ *)

(* compile with:

    ocamlopt -o rusage unix.cmxa rusage.ml

   example:

    ./rusage -sleep 1 -time -pid `pidof whizard`

 *)

module type Statm =
  sig

    type t =
	{ size : int;     (* total program size *)
	  resident : int; (* resident set size *)
	  share : int; 	  (* shared pages *)
	  text : int;  	  (* code size *)
	  lib : int;   	  (* library size (unused in Linux 2.6) *)
	  data : int;  	  (* data + stack size *)
	  dt : int     	  (* dirty pages (unused in Linux 2.6) *) }

    val read : int -> t
    val print : t -> unit

  end

module Statm : Statm =
  struct

    type t =
	{ size : int;     (* total program size *)
	  resident : int; (* resident set size *)
	  share : int; 	  (* shared pages *)
	  text : int;  	  (* code size *)
	  lib : int;   	  (* library size (unused in Linux 2.6) *)
	  data : int;  	  (* data + stack size *)
	  dt : int     	  (* dirty pages (unused in Linux 2.6) *) }

    let of_raw size resident share text lib data dt =
      { size = size;
	resident = resident;
	share = share;
	text = text;
	lib = lib;
	data = data;
	dt = dt }

    let read pid =
      let ic = open_in ("/proc/" ^ string_of_int pid ^ "/statm") in
      let s = Scanf.fscanf ic "%d %d %d %d %d %d %d" of_raw in
      close_in ic;
      s

    open Printf

    let print s =
      printf "size = %d\n" s.size;
      printf "resident = %d\n" s.resident;
      printf "share = %d\n" s.share;
      printf "text = %d\n" s.text;
      printf "lib = %d\n" s.lib;
      printf "data = %d\n" s.data;
      printf "dirty = %d\n" s.dt

  end

module type Stat =
  sig

    type t =
	{ pid : int;           	       	 (* %d *)
	  comm : string;       	       	 (* %s *)
	  state : char;        	       	 (* %c *)
	  ppid : int;          	       	 (* %d *)
	  pgrp : int;          	       	 (* %d *)
	  session : int;       	       	 (* %d *)
	  tty_nr : int;        	       	 (* %d *)
	  tpgid : int;         	       	 (* %d *)
	  flags : int;         	       	 (* %u *)
	  minflt : int;        	       	 (* %lu *)
	  cminflt : int;       	       	 (* %lu *)
	  majflt : int;        	       	 (* %lu *)
	  cmajflt : int;       	       	 (* %lu *)
	  utime : int;         	       	 (* %lu *)
	  stime : int;         	       	 (* %lu *)
	  cutime : int;        	       	 (* %ld *)
	  cstime : int;        	       	 (* %ld *)
	  priority : int;      	       	 (* %ld *)
	  nice : int;          	       	 (* %ld *)
	  num_threads : int;   	       	 (* %ld *)
	  itrealvalue : int;   	       	 (* %ld *)
	  starttime : int64;   	       	 (* %Lu *)
	  vsize : int;         	       	 (* %lu *)
	  rss : int;           	       	 (* %ld *)
	  rsslim : string;     	       	 (* %lu, but too long for signed int64! *) 
	  startcode : string;  	       	 (* %lu, but too long for signed int64! *)
	  endcode : string;    	       	 (* %lu, but too long for signed int64! *)
	  startstack : string; 	       	 (* %lu, but too long for signed int64! *)
	  kstkesp : string;    	       	 (* %lu, but too long for signed int64! *)
	  kstkeip : string;    	       	 (* %lu, but too long for signed int64! *)
	  signal : int;        	       	 (* %lu *)
	  blocked : int;       	       	 (* %lu *)
	  sigignore : int;     	       	 (* %lu *)
	  sigcatch : int;      	       	 (* %lu *)
	  wchan : string;      	       	 (* %lu, but too long for signed int64! *)
	  nswap : int;         	       	 (* %lu *)
	  cnswap : int;        	       	 (* %lu *)
	  exit_signal : int;   	       	 (* %d *)
	  processor : int;     	       	 (* %d *)
	  rt_priority : int;   	       	 (* %u *)
	  policy : int;        	       	 (* %u *)
	  delayacct_blkio_ticks : int64; (* %Lu *)
	  guest_time : int;              (* %lu *)
	  cguest_time : int;             (* %ld *) }

    val read : int -> t
    val print : t -> unit

  end

module Stat : Stat =
  struct

    type t =
	{ pid : int;           	       	 (* %d *)
	  comm : string;       	       	 (* %s *)
	  state : char;        	       	 (* %c *)
	  ppid : int;          	       	 (* %d *)
	  pgrp : int;          	       	 (* %d *)
	  session : int;       	       	 (* %d *)
	  tty_nr : int;        	       	 (* %d *)
	  tpgid : int;         	       	 (* %d *)
	  flags : int;         	       	 (* %u *)
	  minflt : int;        	       	 (* %lu *)
	  cminflt : int;       	       	 (* %lu *)
	  majflt : int;        	       	 (* %lu *)
	  cmajflt : int;       	       	 (* %lu *)
	  utime : int;         	       	 (* %lu *)
	  stime : int;         	       	 (* %lu *)
	  cutime : int;        	       	 (* %ld *)
	  cstime : int;        	       	 (* %ld *)
	  priority : int;      	       	 (* %ld *)
	  nice : int;          	       	 (* %ld *)
	  num_threads : int;   	       	 (* %ld *)
	  itrealvalue : int;   	       	 (* %ld *)
	  starttime : int64;   	       	 (* %Lu *)
	  vsize : int;         	       	 (* %lu *)
	  rss : int;           	       	 (* %ld *)
	  rsslim : string;     	       	 (* %lu, but too long for signed int64! *) 
	  startcode : string;  	       	 (* %lu, but too long for signed int64! *)
	  endcode : string;    	       	 (* %lu, but too long for signed int64! *)
	  startstack : string; 	       	 (* %lu, but too long for signed int64! *)
	  kstkesp : string;    	       	 (* %lu, but too long for signed int64! *)
	  kstkeip : string;    	       	 (* %lu, but too long for signed int64! *)
	  signal : int;        	       	 (* %lu *)
	  blocked : int;       	       	 (* %lu *)
	  sigignore : int;     	       	 (* %lu *)
	  sigcatch : int;      	       	 (* %lu *)
	  wchan : string;      	       	 (* %lu, but too long for signed int64! *)
	  nswap : int;         	       	 (* %lu *)
	  cnswap : int;        	       	 (* %lu *)
	  exit_signal : int;   	       	 (* %d *)
	  processor : int;     	       	 (* %d *)
	  rt_priority : int;   	       	 (* %u *)
	  policy : int;        	       	 (* %u *)
	  delayacct_blkio_ticks : int64; (* %Lu *)
	  guest_time : int;              (* %lu *)
	  cguest_time : int;             (* %ld *) }

    let of_raw pid comm state ppid pgrp session tty_nr tpgid flags
	minflt cminflt majflt cmajflt utime stime cutime cstime priority nice
	num_threads itrealvalue starttime vsize rss rsslim startcode endcode
	startstack kstkesp kstkeip signal blocked sigignore sigcatch wchan
	nswap cnswap exit_signal processor rt_priority policy
	delayacct_blkio_ticks guest_time cguest_time =
      { pid = pid;
	comm = comm;
	state = state;
	ppid = ppid;
	pgrp = pgrp;
	session = session;
	tty_nr = tty_nr;
	tpgid = tpgid;
	flags = flags;
	minflt = minflt;
	cminflt = cminflt;
	majflt = majflt;
	cmajflt = cmajflt;
	utime = utime;
	stime = stime;
	cutime = cutime;
	cstime = cstime;
	priority = priority;
	nice = nice;
	num_threads = num_threads;
	itrealvalue = itrealvalue;
	starttime = starttime;
	vsize = vsize;
	rss = rss;
	rsslim = rsslim;
	startcode = startcode;
	endcode = endcode;
	startstack = startstack;
	kstkesp = kstkesp;
	kstkeip = kstkeip;
	signal = signal;
	blocked = blocked;
	sigignore = sigignore;
	sigcatch = sigcatch;
	wchan = wchan;
	nswap = nswap;
	cnswap = cnswap;
	exit_signal = exit_signal;
	processor = processor;
	rt_priority = rt_priority;
	policy = policy;
	delayacct_blkio_ticks = delayacct_blkio_ticks;
	guest_time = guest_time;
	cguest_time = cguest_time }

    open Printf

    let print s =
      printf "pid = %d\n" s.pid;
      printf "comm = %s\n" s.comm;
      printf "state = %c\n" s.state;
      printf "ppid = %d\n" s.ppid;
      printf "pgrp = %d\n" s.pgrp;
      printf "session = %d\n" s.session;
      printf "tty_nr = %d\n" s.tty_nr;
      printf "tpgid = %d\n" s.tpgid;
      printf "flags = %d\n" s.flags;
      printf "minflt = %d\n" s.minflt;
      printf "cminflt = %d\n" s.cminflt;
      printf "majflt = %d\n" s.majflt;
      printf "cmajflt = %d\n" s.cmajflt;
      printf "utime = %d\n" s.utime;
      printf "stime = %d\n" s.stime;
      printf "cutime = %d\n" s.cutime;
      printf "cstime = %d\n" s.cstime;
      printf "priority = %d\n" s.priority;
      printf "nice = %d\n" s.nice;
      printf "num_threads = %d\n" s.num_threads;
      printf "itrealvalue = %d\n" s.itrealvalue;
      printf "starttime = %Lu\n" s.starttime;
      printf "vsize = %d\n" s.vsize;
      printf "rss = %d\n" s.rss;
      printf "rsslim = %s\n" s.rsslim;
      printf "startcode = %s\n" s.startcode;
      printf "endcode = %s\n" s.endcode;
      printf "startstack = %s\n" s.startstack;
      printf "kstkesp = %s\n" s.kstkesp;
      printf "kstkeip = %s\n" s.kstkeip;
      printf "signal = %d\n" s.signal;
      printf "blocked = %d\n" s.blocked;
      printf "sigignore = %d\n" s.sigignore;
      printf "sigcatch = %d\n" s.sigcatch;
      printf "wchan = %s\n" s.wchan;
      printf "nswap = %d\n" s.nswap;
      printf "cnswap = %d\n" s.cnswap;
      printf "exit_signal = %d\n" s.exit_signal;
      printf "processor = %d\n" s.processor;
      printf "rt_priority = %d\n" s.rt_priority;
      printf "policy = %d\n" s.policy;
      printf "delayacct_blkio_ticks = %Ld\n" s.delayacct_blkio_ticks;
      printf "guest_time = %d\n" s.guest_time;
      printf "cguest_time = %d\n" s.cguest_time

    let read pid =
      let ic = open_in ("/proc/" ^ string_of_int pid ^ "/stat") in
      let s = (* cat /proc/self/stat | tr ' ' '\n' | cat -n *)
	Scanf.fscanf ic
	  "%d (%s@) %c %d %d %d %d %d %u %u \
	  %u %u %u %u %u %d %d %d %d %d \
	  %d %Lu %u %d %s %s %s %s %s %s \
	  %u %u %u %u %s %u %u %d %d %u \
	  %u %Lu %u %d" of_raw in
      close_in ic;
      s

  end

module type Process =
  sig

    type id = int

    val map : (id -> 'a) -> unit -> 'a list
    val iter : (id -> unit) -> unit -> unit
    val fold : (id -> 'a -> 'a) -> 'a -> 'a

    module Table : Map.S with type key = id
    val table : unit -> Stat.t Table.t
    type tree_node = { stat : Stat.t; children : id list }
    val tree : Stat.t Table.t -> tree_node Table.t
    val print_tree : tree_node Table.t -> unit
    val collect_offspring : tree_node Table.t -> id -> id list

  end

module Process : Process =
  struct

    type id = int

    let all () =
      List.fold_left
	(fun acc name -> try int_of_string name :: acc with Failure "int_of_string" -> acc)
	[] (Array.to_list (Sys.readdir "/proc/"))

    let map f () =
      List.map f (all ())

    let iter f () =
      List.iter f (all ())

    let fold f initial =
      List.fold_right f (all ()) initial

    module Table = Map.Make (struct type t = id let compare = compare end)

    let table () =
      fold
	(fun pid acc -> try Table.add pid (Stat.read pid) acc with _ -> acc)
	Table.empty

    type tree_node = { stat : Stat.t; children : id list }

    let tree ptable =
      let empty = Table.map (fun s -> { stat = s; children = [] }) ptable in
      Table.fold
	(fun pid process acc ->
	  try
	    let mother = Table.find process.stat.Stat.ppid acc in
	    Table.add
	      mother.stat.Stat.pid
	      { mother with children = pid :: mother.children }
	      acc
	  with
	  | Not_found -> acc )
	empty empty

    let print_tree ptree =
      Table.iter
	(fun p { stat = s; children = ch } ->
	  Printf.printf "%d (%s) -> %s\n"
	    s.Stat.pid s.Stat.comm (String.concat ", " (List.map string_of_int ch)))
	ptree

    let rec collect_offspring ptree pid =
      let process = Table.find pid ptree in
      pid :: List.flatten (List.map (collect_offspring ptree) process.children)

  end

module type Rusage =
  sig

    type t =
	{ size : int;
	  resident : int;
	  user : int;
	  kernel : int }

    val of_stat : Stat.t -> t
    val to_string : t -> string
    val update : t -> t -> t

    module Table : Map.S with type key = Process.id * string

    val update_table : Process.id list -> Stat.t Process.Table.t -> t Table.t -> t Table.t

    val by_size : t Table.t -> (Process.id * string * t) list
    val by_time : t Table.t -> (Process.id * string * t) list

    val print_list : (Process.id * string * t) list -> unit

  end

module Rusage : Rusage =
  struct

    type t =
	{ size : int;
	  resident : int;
	  user : int;
	  kernel : int }

    let of_stat s =
      { size = s.Stat.vsize;
	resident = s.Stat.rss;
	user = s.Stat.utime;
	kernel = s.Stat.stime }

    let size_to_mb size = size / 1024 / 1024
    let resident_to_mb resident = resident * 4 / 1024 (* probably NOT portable!!! *)
    let ticks_to_secs ticks = ticks / 100 (* probably NOT portable!!! *)

    let to_string ru =
      let total_seconds = ticks_to_secs (ru.user + ru.kernel) in
      let seconds = total_seconds mod 60 in
      let total_minutes = (total_seconds - seconds) / 60 in
      let minutes = total_minutes mod 60 in
      let hours = (total_minutes - minutes) / 60 in
      Printf.sprintf
	"size = %d MB, rss = %d MB, time = %d sec (%d:%02d:%02d, kernel = %d%%)"
	(size_to_mb ru.size)
	(resident_to_mb ru.resident) 
	total_seconds
	hours minutes seconds
	(try 100 * ru.kernel / (ru.user + ru.kernel) with _ -> 0)

    let update ru_old ru_new =
      { size = max ru_old.size ru_new.size;
	resident = max ru_old.resident ru_new.resident;
	user = max ru_old.user ru_new.user;
	kernel = max ru_old.kernel ru_new.kernel}

    module Table =
      Map.Make (struct type t = Process.id * string let compare = compare end)

    let update_table plist ptable ru_table =
      List.fold_right
	(fun pid acc ->
	  let s = Process.Table.find pid ptable in
	  let pid' = (pid, s.Stat.comm)
	  and ru_old = of_stat s in
	  let ru_new =
	    try
	      update (Table.find pid' acc) ru_old
	    with
	    | Not_found -> ru_old in
	  Table.add pid' ru_new acc)
	plist ru_table
	
    let to_list ru_table =
      Table.fold (fun (pid, cmd) ru acc -> (pid, cmd, ru) :: acc) ru_table []

    let by_size ru_table =
      List.sort
	(fun (_, _, ru1 as p1) (_, _, ru2 as p2) ->
	  let c = compare ru1.size ru2.size in
	  if c <> 0 then
	    - c
	  else
	    compare p1 p2)
	(to_list ru_table)

    let by_time ru_table =
      List.sort
	(fun (_, _, ru1 as p1) (_, _, ru2 as p2) ->
	  let c = compare (ru1.user + ru1.kernel) (ru2.user + ru2.kernel) in
	  if c <> 0 then
	    - c
	  else
	    compare p1 p2)
	(to_list ru_table)

    let print_list ru_list =
      List.iter
	(fun (pid, cmd, ru) -> Printf.printf "%s (%d): %s\n" cmd pid (to_string ru))
	ru_list

  end

let pid_of_opt = function
  | None -> 1
  | Some pid -> pid

let print_separator () =
  Printf.printf
    "================================================================================\n"

let _ =
  let usage = Sys.argv.(0) ^ " [options]"
  and sorting = ref Rusage.by_time
  and pid = ref None
  and quiet = ref false
  and sleep = ref None in
  Arg.parse
    [ ("-size", Arg.Unit (fun () -> sorting := Rusage.by_size), "");
      ("-time", Arg.Unit (fun () -> sorting := Rusage.by_time), "");
      ("-pid", Arg.Int (fun p -> pid := Some p), "");
      ("-quiet", Arg.Set quiet, "");
      ("-sleep", Arg.Int (fun s -> sleep := Some s), "") ]
    (fun _ -> ())
    usage;
  match !sleep with
  | None ->
      let processes = Process.table () in
      let offspring =
	Process.collect_offspring (Process.tree processes) (pid_of_opt !pid) in
      Rusage.print_list
	(!sorting (Rusage.update_table offspring processes Rusage.Table.empty))
  | Some seconds ->
      let offspring_rusage = ref Rusage.Table.empty in
      let processes = ref (Process.table ()) in
      let pid = pid_of_opt !pid in
      while Process.Table.mem pid !processes do
	let offspring = Process.collect_offspring (Process.tree !processes) pid in
	offspring_rusage := Rusage.update_table offspring !processes !offspring_rusage;
	if not !quiet then begin
	  print_separator ();
	  Rusage.print_list (!sorting !offspring_rusage);
	  flush stdout;
	end;
	Unix.sleep seconds;
	processes := Process.table ()
      done;
      if not !quiet then
	print_separator ();
      Rusage.print_list (!sorting !offspring_rusage)
      
