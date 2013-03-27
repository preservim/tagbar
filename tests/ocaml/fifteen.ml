(* $Id: fifteen.ml,v 1.8 2001/09/06 08:47:55 garrigue Exp $ *)

open StdLabels
open Gaux
open Gtk
open GObj
open GMain

class position ~init_x ~init_y ~min_x ~min_y ~max_x ~max_y = object
  val mutable x = init_x
  val mutable y = init_y
  method current = (x, y)
  method up ()    = if y > min_y then y <- y-1 else (); (x, y)
  method down ()  = if y < max_y then y <- y+1 else (); (x, y)
  method left ()  = if x > min_x then x <- x-1 else (); (x, y)
  method right () = if x < max_x then x <- x+1 else (); (x, y)
end

let game_init () = (* generate initial puzzle state *)
  let rec game_aux acc rest n_invert =
    let len = List.length rest in
    if len=0 then
      if n_invert mod 2 = 0 then
        acc (* to be solvable, n_invert must be even *)
      else
        (List.hd (List.tl acc))::(List.hd acc)::(List.tl (List.tl acc))
    else begin
      let rec extract n xs =
        if (n=0) then (List.hd xs, List.tl xs)
        else
          let (ans, ys) = extract (n-1) (List.tl xs) in
          (ans, List.hd xs :: ys) in
      let ran = Random.int len in
      let (elm, rest1) = extract ran rest in
      let rec count p xs = match xs with
        [] -> 0
      | y :: ys -> let acc = count p ys in
                 if p y then 1+acc else acc
      in
      let new_n_invert = count (fun x -> elm > x) acc in
      game_aux (elm :: acc) rest1 (n_invert+new_n_invert)
    end in
  let rec from n = if n=0 then [] else n :: from (n-1) in
  game_aux [] (from 15) 0

let _ = Random.init (int_of_float (Sys.time () *. 1000.))
let window = GWindow.window ()
let _ = window#connect#destroy ~callback:GMain.Main.quit

let tbl = GPack.table ~rows:4 ~columns:4 ~homogeneous:true ~packing:window#add ()
let dummy = GMisc.label ~text:"" ~packing:(tbl#attach ~left:3 ~top:3) ()
let arr = Array.create_matrix ~dimx:4 ~dimy:4 dummy
let init = game_init ()
let _ =
  for i = 0 to 15 do
    let j = i mod 4  in
    let k = i/4 in
    let frame =
      GBin.frame ~shadow_type:`OUT ~width:32 ~height:32
        ~packing:(tbl#attach ~left:j ~top:k) () in
    if i < 15 then
      arr.(j).(k) <-
        GMisc.label ~text:(string_of_int (List.nth init i))
          ~packing:frame#add ()
  done
let pos = new position ~init_x:3 ~init_y:3 ~min_x:0 ~min_y:0 ~max_x:3 ~max_y:3

open GdkKeysyms

let _ =
  window#event#connect#key_press ~callback:
    begin fun ev ->
      let (x0, y0) = pos#current in
      let wid0 = arr.(x0).(y0) in
      let key = GdkEvent.Key.keyval ev in
      if key = _q || key = _Escape then (Main.quit (); exit 0) else
      let (x1, y1) =
        if key = _h || key = _Left then
          pos#right ()
        else if key = _j || key = _Down then
          pos#up ()
        else if key = _k || key = _Up then
          pos#down ()
        else if key = _l || key = _Right then
          pos#left ()
        else (x0, y0)
      in
      let wid1 = arr.(x1).(y1) in
      wid0#set_text (wid1#text);
      wid1#set_text "";
      true
    end

let main () =
  window#show ();
  Main.main ()

let _ = main ()
