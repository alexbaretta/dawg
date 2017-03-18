module IntSet = Utils.IntSet

type mprocess = {
  pids : IntSet.t;
}

type t =
  | MProcess of mprocess

let create_mprocess () = MProcess { pids = IntSet.empty }

let spawn mcore f x =
  match mcore with
    | MProcess { pids } ->
      let pid = Unix.fork () in
      match pid with
        | 0 -> f x; exit 0
        | pid ->
          let pids = IntSet.add pid pids in
          MProcess { pids }

let sync mcore =
  match mcore with
    | MProcess { pids } ->
      IntSet.fold (fun pid accu ->
        match Unix.waitpid [] pid with
          | pid_, status ->
            assert (pid_ = pid);
            match status with
              | Unix.WEXITED(0) -> true && accu
              | Unix.WEXITED(_) -> false
              | Unix.WSIGNALED(_) -> false
              | Unix.WSTOPPED(_) -> assert false
      ) pids true


let reap_mprocess accu pids =
  IntSet.fold (fun pid (accu, pids) ->
    match Unix.(waitpid [WNOHANG] pid) with
      | 0, _ -> (true && accu), pids
      | pid_, status ->
        assert (pid_ = pid);
        let pids = IntSet.remove pid pids in
        let accu = match status with
          | Unix.WEXITED(0) -> true && accu
          | Unix.WEXITED(_) -> false
          | Unix.WSIGNALED(_) -> false
          | Unix.WSTOPPED(_) -> assert false
        in
        accu, pids
  ) pids (true, pids)

let reap mcore =
  match mcore with
    | MProcess { pids } ->
      let result, pids = reap_mprocess true pids in
      let mcore = MProcess { pids } in
      result, mcore
