type ordering = LT | EQ | GT;;

module type ORDER =
  sig
    type t
    val compare: t -> t -> ordering
  end;;


module StringOrder: ORDER with type t = (int * int) =
  struct
    type t = int * int
    let compare s1 s2 = if fst(s1) < fst(s2) then LT
                        else if fst(s1) > fst(s2) then GT
                        else
                          if snd(s1) < snd(s2) then LT
                          else if snd(s1) < snd(s2) then GT
                          else EQ
  end;;

module type KOLEJKA =
  sig
    type typ
    type 'a tk
    exception Pusta of string
    val tworz_pusta: unit -> 'a tk
    val do_kolejki: 'a * 'a tk -> 'a tk
    val z_kolejki: 'a tk -> 'a tk
    val pierwszy_element: 'a tk -> 'a
    val czy_pusta: 'a tk -> bool
    val do_listy: 'a tk -> 'a list
    val elementy_spelniajace_warunek: 'a tk -> 'a -> typ -> int
    
  end;;

module Kolejka (Typ:ORDER) : KOLEJKA =
  struct
    type typ = Typ.t
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    exception Pusta of string

    let tworz_pusta() = KolejkaPusta
    
    let do_kolejki(e, queue) = Skladowa(e, queue)
    
    let z_kolejki queue =
        match queue with
        | Skladowa(h, t) -> t
        | _ -> KolejkaPusta
    
    let pierwszy_element queue =
        match queue with
        | Skladowa(h, t) -> h
        | _ -> raise (Pusta "kolejka pusta")
    
    let kolejka_pusta queue =
        match queue with
        | KolejkaPusta -> true
        | _ -> false
    
    let do_listy queue  =
      let rec do_listy_rec remaining res =
        match remaining with
        | Skladowa(h, t) -> do_listy_rec (z_kolejki remaining) (h::res)
        | KolejkaPusta -> (List.rev res)
      in do_listy_rec queue []
    
    let elementy_spelniajace_warunek queue w op =
      let elementy_spelniajace_warunek_rec remaining res =
        match remaining with
        |Skladowa(h, t) -> {match Typ.compare(h w) with
                           |op -> elementy_spelniajace_warunek_rec t (res+1)
                           |_ -> elementy_spelniajace_warunek_rec t res
                           }
        |_ -> res
      in elementy_spelniajace_warunek_rec queue 0
    let convert op =
      match op with
      |(<) -> LT
      |(<=) -> (LT|EQ)
      |(>=) -> (EQ|GT)
      |(>) -> GT
end;;



