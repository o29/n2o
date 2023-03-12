Require Import Coq.Lists.List.
Require Import Io.All.
Require Import String.
Require Import Io.System.All.
Require Import ListString.All.
        Import ListNotations.

  CoInductive Co (E : Effect.t) : Type -> Type :=
    | Ret : forall (A : Type) (x : A), Co E A
    | Bind : forall (A B : Type), Co E A -> (A -> Co E B) -> Co E B
    | Call : forall (command : Effect.command E), Co E (Effect.answer E command)
    | Split : forall (A : Type), Co E A -> Co E A -> Co E A
    | Join : forall (A B : Type), Co E A -> Co E B -> Co E (A * B).

  Arguments Ret {E} _ _.
  Arguments Bind {E} _ _ _ _.
  Arguments Call {E} _.
  Arguments Split {E} _ _ _.
  Arguments Join {E} _ _ _ _.

  Definition ret   {E : Effect.t} {A : Type} (x : A) : Co E A := Ret A x.
  Definition split {E : Effect.t} {A : Type} (x1 x2 : Co E A) : Co E A := Split A x1 x2.
  Definition join  {E : Effect.t} {A B : Type} (x : Co E A) (y : Co E B): Co E (A * B) := Join A B x y.
  Definition call  (E : Effect.t) (command : Effect.command E):
                   Co E (Effect.answer E command) := Call (E := E) command.

  Notation "'ilet!' x ':=' X 'in' Y" := (Bind _ _ X (fun x => Y))
           (at level 200, x ident, X at level 100, Y at level 200).

  Notation "'ilet!' x : A ':=' X 'in' Y" := (Bind _ _ X (fun (x : A) => Y))
           (at level 200, x ident, X at level 100, A at level 200, Y at level 200).

  Notation "'ido!' X 'in' Y" := (Bind _ _ X (fun (_ : unit) => Y))
           (at level 200, X at level 100, Y at level 200).

  Definition read_line : Co effect (option LString.t) :=
    call effect ReadLine.

  Definition printl (message : LString.t) : Co effect bool :=
    call effect (Print (message ++ [LString.Char.n])).

  Definition log (message : LString.t) : Co effect unit :=
    ilet! is_success := printl message in
    ret tt.

  Definition run (argv : list LString.t): Co effect unit :=
    ido! log (LString.s "What is your name?") in
    ilet! name := read_line in
    match name with
      | None => ret tt
      | Some name => log (LString.s "Hello " ++ name ++ LString.s "!")
    end.

  Parameter infinity : nat.
  Parameter error : forall {A B}, A -> B.

  Fixpoint eval_aux {A} (steps : nat) (x : Co effect A) : Lwt.t A :=
    match steps with
      | O => error tt
      | S steps =>
        match x with
          | Ret _ v => Lwt.ret v
          | Call c => eval_command c
          | Bind _ _ x f => Lwt.bind (eval_aux steps x) (fun v_x => eval_aux steps (f v_x))
          | Split _ x1 x2 => Lwt.choose (eval_aux steps x1) (eval_aux steps x2)
          | Join _ _ x y => Lwt.join (eval_aux steps x) (eval_aux steps y)
        end
    end.

  Definition eval {A} (x : Co effect A) : Lwt.t A :=
    eval_aux infinity x.

  CoFixpoint handle_commands : Co effect unit :=
    ilet! name := read_line in
    match name with
      | None => ret tt
      | Some command =>
        ilet! result := log (LString.s "Input: " ++ command ++ LString.s ".") in
        handle_commands
    end.

  Definition launch (m : list LString.t -> Co effect unit): unit :=
    let argv := List.map String.to_lstring Sys.argv in
    Lwt.launch (eval (m argv)).

  Definition corun (argv : list LString.t): Co effect unit :=
    handle_commands.

  Definition main :=
    launch corun. (* launch run. *)

Extract Constant infinity => "let rec inf = S inf in inf".
Extract Constant error => "fun _ -> failwith ""Unexpected end""".
Extraction "extraction/main" main.
