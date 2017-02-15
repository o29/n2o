Require Import Coq.Lists.List.
Require Import Io.All.
Require Import Io.System.All.
Require Import ListString.All.

Import ListNotations.
Import C.Notations.

Definition run (argv : list LString.t)
  : C.t System.effect unit :=
  let! _ : unit * unit := join
    (System.log (LString.s "Hello"))
    (System.log (LString.s "World")) in
  ret tt.

Definition main := Extraction.launch run.
Extraction "extraction/main" main.

Module Run.
  Import Io.Run.
  Definition run_ok (argv : list LString.t)
    : Run.t (run argv) tt.
    apply (Let (Join
      (Run.log_ok (LString.s "Hello"))
      (Run.log_ok (LString.s "World")))).
    apply Ret.
  Defined.
End Run.
