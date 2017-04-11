module IntSet = Utils.IntSet

let epr = Utils.pr

type mprocess = {
  max_threads : int;
  mutable n_threads : int;
  mutable pids : IntSet.t;
}

type t =
  | MProcess of mprocess
  | MSynchronous

let create_mprocess max_threads =
  if max_threads < 1 then
    Printf.ksprintf invalid_arg "create_mprocess: max_threads=%d" max_threads
  else
    MProcess {
      max_threads;
      n_threads = 0;
      pids = IntSet.empty
    }

let create_msynchronous () = MSynchronous

let list mcore = match mcore with
  | MProcess { pids } ->
    IntSet.fold (fun pid accu -> pid :: accu) pids []
  | MSynchronous -> []

let check_status pid status =
  match status with
    | Unix.WEXITED(0) -> true
    | Unix.WEXITED(n) ->
      epr "[ERROR][mcore] pid %d exited with status %d\n%!" pid n;
      false
    | Unix.WSIGNALED(n) ->
      epr "[ERROR][mcore] pid %d received signal %d\n%!" pid n;
      false
    | Unix.WSTOPPED(_) -> assert false

let rec wait_available_mprocess mprocess =
  let { n_threads; max_threads; pids } = mprocess in
  assert (n_threads <= max_threads);
  if n_threads = max_threads then
    match Unix.waitpid [] 0 with
      | pid, status ->
        mprocess.pids <- IntSet.remove pid pids;
        mprocess.n_threads <- pred n_threads;
        check_status pid status
  else
    true


let sync mcore =
  match mcore with
    | MProcess mprocess ->
      IntSet.fold (fun pid accu ->
        match Unix.waitpid [] pid with
          | pid_, status ->
            assert (pid_ = pid);
            mprocess.pids <- IntSet.remove pid mprocess.pids;
            mprocess.n_threads <- pred mprocess.n_threads;
            check_status pid status
      ) mprocess.pids true
    | MSynchronous -> true

let rec reap_mprocess accu mprocess =
  let wait_result =
    try
      Some(Unix.(waitpid [WNOHANG] 0))
    with Unix.Unix_error (Unix.ECHILD, _, _)  -> None
  in
  match wait_result with
    | None
    | Some(0, _) -> accu
    | Some(pid, status) ->
      mprocess.pids <- IntSet.remove pid mprocess.pids;
      mprocess.n_threads <- pred mprocess.n_threads;
      let result = check_status pid status in
      let accu = result && accu in
      reap_mprocess accu mprocess

let reap mcore =
  match mcore with
    | MProcess mprocess ->
      reap_mprocess true mprocess
    | MSynchronous -> true

let wait_available mcore = match mcore with
  | MProcess mprocess -> wait_available_mprocess mprocess
  | MSynchronous -> true

let spawn mcore f x =
  let result = reap mcore && wait_available mcore in
  if not result then
    epr "[ERROR] a thread died unsuccessfully\n%!";
  match mcore with
    | MProcess mprocess -> (
      let pid = Unix.fork () in
      match pid with
        | 0 -> f x; exit 0
        | pid ->
          mprocess.n_threads <- succ mprocess.n_threads;
          mprocess.pids <- IntSet.add pid mprocess.pids
    )
    | MSynchronous -> f x
