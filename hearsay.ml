open Containers

module Tile = struct
  type layout = { size : Raylib.Vector2.t; origin : Raylib.Vector2.t }

  type t = { x : int; y : int }

  (* TODO test  *)
  let from_px x y layout =
    let open Raylib in
    {
      x = Int.of_float ((x -. Vector2.x layout.origin) /. Vector2.x layout.size);
      y = Int.of_float ((y -. Vector2.y layout.origin) /. Vector2.y layout.size);
    }

  let vec_of_tl tl layout =
    let open Raylib in
    Vector2.create
      ((Float.of_int tl.x *. Vector2.x layout.size) +. Vector2.x layout.origin)
      ((Float.of_int tl.y *. Vector2.y layout.size) +. Vector2.y layout.origin)
end

module Easings = struct
  let linear ~t ~start ~final ~dur = ((final -. start) *. t /. dur) +. start

  let cubic_in ~t ~start ~final ~dur =
    let t = t /. dur in
    ((final -. start) *. t *. t *. t) +. start

  let quad_in_out ~t ~start ~final ~dur =
    let final = final -. start in
    let t = t /. dur *. 2.0 in
    if t <. 1.0 then (final /. 2.0 *. (t *. t)) +. start
    else (-.final /. 2.0 *. (((t -. 1.0) *. (t -. 3.0)) -. 1.0)) +. start
end

module Queue = struct
  type kind = Entity of Entities.index | Turn

  type item = int * kind

  type t = item list

  let leq (a, _) (b, _) = a <= b

  let fake_cmp (a, _) (b, _) = if a < b then -1 else 1

  let insert item lst = List.sorted_insert ~cmp:fake_cmp item lst

  let head = function
    | (_, Entity ent) :: _ -> Some (Entity ent)
    | (_, Turn) :: _ -> Some Turn
    | [] -> None

  let advance_head lst outcome =
    match (outcome, lst) with
    | `Enqueue p, (prio, kind) :: tl -> insert (prio + p, kind) tl
    | `Drop, _ :: tl -> tl
    | _, [] -> []
end

let width = 1280

let height = 720

let bar_time = 3.0

let bar_width = Float.of_int width /. 2.0

let center_of_width w = (width / 2) - (w / 2)

type bar = {
  fraction : float;
  width : int;
  border : int;
  height : int;
  outerc : Raylib.Color.t;
  innerc : Raylib.Color.t;
}

type skillstate =
  | Cooldown of float
  | Casting of float * Entities.index option
  | Ready

type effect = Damage of int | Start_Dodge of float

type skillname = Slap | Dodge

let str_of_skilltyp = function Slap -> "slap" | Dodge -> "dodge"

type target_type = Self | Single_target

type skill = {
  typ : skillname;
  target_type : target_type;
  range : int;
  state : skillstate;
  effect : effect;
  cooldown : float;
  casttime : float;
  possible : bool; (* This should really be done differently TODO *)
}

type effect_container = {
  effect : effect;
  caster : Entities.index;
  target : Entities.index option;
}

type health = { current : int; max : int }

type component = Is_Aggro of bool | Dodging of float | Health of health

type direction = Left | Right | Up | Down

type status = Moving of direction * float * float | Casting | Idle

type entity = {
  position : Tile.t;
  status : status;
  target : Entities.index option;
  is_player : bool;
  comps : component list;
  skills : skill list;
  texture : Raylib.Texture2D.t;
}

type ui = {
  player : Entities.index;
  healthbar : bar;
  skills : (bar * skillname) list;
  ui : ui option;
}

let fraction_width b =
  let smallest_fraction = 1.0 /. Float.of_int b.width in
  Int.of_float
    ( Float.of_int (b.width - (4 * b.border))
    *. (b.fraction +. smallest_fraction) )

let draw_bar b x y =
  (* draw outer border *)
  Raylib.draw_rectangle x y b.width b.height b.outerc;
  (* draw inner border *)
  Raylib.draw_rectangle (x + b.border) (y + b.border)
    (b.width - (2 * b.border))
    (b.height - (2 * b.border))
    Raylib.Color.raywhite;
  Raylib.draw_rectangle
    (x + (2 * b.border))
    (y + (2 * b.border))
    (fraction_width b)
    (b.height - (4 * b.border))
    b.innerc;
  ()

(* Some skills should only be castable if they have target.. some skills need a target  *)

let draw_bar_text b x y txt sz =
  let open Raylib in
  (* let b = { b with innerc = Color.lightgray; outerc = Color.lightgray } in *)
  draw_bar b x y;
  let bar_start = x + (2 * b.border) in
  let bar_width = b.width - (4 * b.border) in
  let fracw = fraction_width b in
  let w = measure_text txt sz in
  let y = y + (b.height / 2) - (sz / 2) in
  let x = x + (b.width / 2) - (w / 2) in
  (* cut to fraction *)
  begin_scissor_mode bar_start y fracw sz;
  draw_text txt x y sz Color.raywhite;
  end_scissor_mode ();
  (* cut from end of fraction *)
  begin_scissor_mode (bar_start + fracw) y bar_width sz;
  draw_text txt x y sz b.innerc;
  end_scissor_mode ();
  ()

let activate_bar bar = function
  | true -> Raylib.{ bar with innerc = Color.black; outerc = Color.black }
  | false -> Raylib.{ bar with innerc = Color.gray; outerc = Color.gray }

let handle_skill skill dt should_cast status target =
  let target, can_cast =
    match (target, skill.target_type) with
    | _, Self -> (None, true)
    | None, Single_target -> (None, false)
    | Some (target, dist), Single_target -> (Some target, dist <= skill.range)
  in
  match (skill.state, status) with
  | Ready, Idle ->
      if should_cast && can_cast then
        ( Casting,
          None,
          { skill with state = Casting (0.0, target); possible = true } )
      else (status, None, { skill with possible = can_cast })
  | Ready, _ -> (status, None, skill)
  | Cooldown t, _ ->
      let t = t -. dt in
      ( status,
        None,
        if t <=. 0.0 then { skill with state = Ready; possible = can_cast }
        else { skill with state = Cooldown t } )
  | Casting (t, target), Casting ->
      let t = t +. dt in
      if t >=. skill.casttime then
        ( Idle,
          Some (skill.effect, target),
          { skill with state = Cooldown skill.cooldown; possible = false } )
      else (Casting, None, { skill with state = Casting (t, target) })
  | Casting _, _ -> assert false

(* entity should really be casting *)

let is_dodging states =
  List.exists (function Dodging _ -> true | _ -> false) states

let handle_ev ~(caster : entity) ~(target : entity) = function
  | Damage dmg ->
      if not (is_dodging target.comps) then
        (* TODO should maybe fire new event to make aggroing less magic? *)
        let comps =
          List.map
            (function
              | Is_Aggro _ -> Is_Aggro true
              | Health health ->
                  Health { health with current = health.current - dmg }
              | any -> any)
            target.comps
        in
        (caster, { target with comps })
      else (caster, target)
  | Start_Dodge time ->
      Printf.printf "start dodge\n%!";
      ({ caster with comps = Dodging time :: caster.comps }, target)

let keymap = function
  | Slap -> Raylib.(is_key_pressed Key.Q)
  | Dodge -> Raylib.(is_key_pressed Key.W)

(* This should be layout_center *)
(* let layout_skills skills =
 *   let n = List.length skills in
 *   List.mapi
 *     (fun i (s : skill) ->
 *       (width / 2) - (n * (s.bar.width / 2)) + (i * (s.bar.width + s.bar.border)))
 *     skills *)

let layout_bars_left x bars =
  let _, bars =
    List.fold_map (fun x bar -> (x + bar.width + (bar.border * 2), x)) x bars
  in
  bars

let layout_bars_right x bars =
  let _, bars =
    List.fold_map
      (fun x bar -> (x - bar.width - (bar.border * 2), x - bar.width))
      x bars
  in
  List.rev bars

let handle_skills dt cast_pred skills state dist_target =
  (* generally, has target and has sight of target could be passed in here *)
  let (status, evs), skills =
    List.fold_map
      (fun (status, evs) s ->
        let status, ev, skill =
          handle_skill s dt (cast_pred s) status dist_target
        in
        let evs = match ev with Some ev -> ev :: evs | None -> evs in
        ((status, evs), skill))
      (state, []) skills
  in
  (status, evs, skills)

let move dt layout ent =
  let open Raylib in
  match ent.status with
  | Idle | Casting -> (Tile.vec_of_tl ent.position layout, ent)
  | Moving (dir, cur, dur) ->
      (* TODO 0.5 if *)
      let start = Tile.vec_of_tl ent.position layout in
      let final_tl =
        match dir with
        | Left -> { ent.position with x = ent.position.x - 1 }
        | Right -> { ent.position with x = ent.position.x + 1 }
        | Up -> { ent.position with y = ent.position.y - 1 }
        | Down -> { ent.position with y = ent.position.y + 1 }
      in
      let final = Tile.vec_of_tl final_tl layout in
      let t = Float.min dur (cur +. dt) in
      let current =
        Vector2.create
          (Easings.quad_in_out ~t ~start:(Vector2.x start)
             ~final:(Vector2.x final) ~dur)
          (Easings.quad_in_out ~t ~start:(Vector2.y start)
             ~final:(Vector2.y final) ~dur)
      in

      ( current,
        if t =. dur then { ent with position = final_tl; status = Idle }
        else { ent with status = Moving (dir, t, dur) } )

let pseudo_dist p1 p2 = Tile.(max (abs (p1.x - p2.x)) (abs (p1.y - p2.y)))

let create_ui ?(innerc = Raylib.Color.green) player =
  {
    player;
    healthbar =
      {
        fraction = 1.0;
        width = 300;
        border = 3;
        height = 70;
        innerc;
        outerc = Raylib.Color.black;
      };
    skills = [];
    ui = None;
  }

let update_ui ui player =
  let healthbar =
    match
      List.find_opt (function Health _ -> true | _ -> false) player.comps
    with
    | Some (Health h) ->
        {
          ui.healthbar with
          fraction = Float.of_int h.current /. Float.of_int h.max;
        }
    | None | Some _ ->
        (* TODO log *)
        Printf.printf "alleged player has no heatlh\n";
        ui.healthbar
  in

  let rec update skills bars updated_bars =
    let get_fraction skill =
      match skill.state with
      | Cooldown t -> t /. skill.cooldown
      | Casting (t, _) -> t /. skill.casttime
      | Ready -> 0.0
    in

    match (skills, bars) with
    | [], [] -> List.rev updated_bars
    | skill :: stl, (bar, n) :: btl ->
        update stl btl
          ( ( {
                (activate_bar bar skill.possible) with
                fraction = get_fraction skill;
              },
              n )
          :: updated_bars )
    | [], _ :: _ -> List.rev updated_bars
    | skill :: stl, [] ->
        update stl []
          ( ( activate_bar
                {
                  fraction = get_fraction skill;
                  width = 100;
                  border = 3;
                  height = 70;
                  innerc = Raylib.Color.black;
                  outerc = Raylib.Color.black;
                }
                skill.possible,
              skill.typ )
          :: updated_bars )
  in
  let skills = update player.skills ui.skills [] in
  { ui with healthbar; skills }

let draw_ui ui =
  let margin = 20 in
  let player_bar_x = margin in
  let player_bar_y = margin in

  draw_bar ui.healthbar player_bar_x player_bar_y;
  let skill_y = margin + ui.healthbar.height + (ui.healthbar.border * 2) in
  let xs =
    layout_bars_left margin (List.map (fun (skill, _) -> skill) ui.skills)
  in
  List.iter2
    (fun x (bar, name) -> draw_bar_text bar x skill_y (str_of_skilltyp name) 30)
    xs ui.skills;

  let target_bar_x = player_bar_x + ui.healthbar.width + margin in
  match ui.ui with
  | Some ui ->
      draw_bar ui.healthbar target_bar_x player_bar_y;
      let xs =
        layout_bars_right
          (target_bar_x + ui.healthbar.width)
          (List.map (fun (skill, _) -> skill) ui.skills)
      in
      List.iter2
        (fun x (bar, name) ->
          draw_bar_text bar x skill_y (str_of_skilltyp name) 30)
        xs ui.skills
  | None -> ()

let tick_ent entity _ =
  if entity.is_player then
    if Raylib.(is_key_pressed Key.Left) then ([], `Player 10) else ([], `Not_yet)
  else (
    Printf.printf "next ent\n%!";
    ([], `Npc 10) )

let setup () =
  let open Raylib in
  set_config_flags [ ConfigFlag.Window_resizable ];
  init_window width height "raylib hearsay";
  set_target_fps 60;
  let skill =
    {
      typ = Slap;
      target_type = Single_target;
      range = 1;
      state = Ready;
      cooldown = 1.5;
      effect = Damage 20;
      casttime = 0.5;
      possible = false;
    }
  in
  let enemyskill = { skill with casttime = 1.5; cooldown = 2.0 } in
  let dodge =
    {
      skill with
      typ = Dodge;
      target_type = Self;
      casttime = 0.1;
      cooldown = 4.0;
      effect = Start_Dodge 0.3;
    }
  in

  let enemy =
    {
      position = { x = 2; y = -2 };
      status = Idle;
      comps = [ Is_Aggro false; Health { current = 60; max = 60 } ];
      is_player = false;
      skills = [ enemyskill ];
      target = None;
      texture = Raylib.load_texture "data/medievalUnit_09.png";
    }
  in
  let player =
    {
      position = { x = 1; y = 0 };
      status = Idle;
      comps = [ Health { current = 100; max = 100 } ];
      is_player = true;
      target = None;
      skills = [ skill; dodge ];
      texture = Raylib.load_texture "data/medievalUnit_01.png";
    }
  in
  let layout =
    {
      Tile.size = Vector2.create 60.0 60.0;
      origin =
        Vector2.create (Float.of_int width /. 2.0) (Float.of_int height /. 2.0);
    }
  in

  let entities = Entities.create_container () in
  let iplayer, entities = Entities.create_entity entities player in
  let e1, entities = Entities.create_entity entities enemy in
  let e2, entities =
    Entities.create_entity entities { enemy with position = { x = -2; y = -2 } }
  in
  let e3, entities =
    Entities.create_entity entities { enemy with position = { x = 2; y = 2 } }
  in

  let ui = create_ui iplayer in

  let turn_queue =
    List.stable_sort
      (fun (a, _) (b, _) -> Int.compare a b)
      Queue.
        [
          (0, Entity iplayer);
          (0, Entity e1);
          (0, Entity e2);
          (0, Entity e3);
          (10, Turn);
        ]
  in

  (entities, layout, ui, turn_queue)

let rec loop entities layout ui turn_queue =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      let dt = get_frame_time () in

      (* TODO pick event here  *)
      let positions, _ =
        Entities.fold_map
          (fun (positions, events) _ entity ->
            (* if this returns only an event, then how will we deal with
             * situations where it wants to cast after?
             * this needs to be passed in somehow *)
            (* this does not really work for targeting *)
            (* let status =
             *   if entity.is_player then
             *     match
             *       match entity.status with
             *       | Idle ->
             *           if is_key_down Key.Right then Some Right
             *           else if is_key_down Key.Left then Some Left
             *           else if is_key_down Key.Up then Some Up
             *           else if is_key_down Key.Down then Some Down
             *           else None
             *       | Moving _ | Casting -> None
             *     with
             *     | None -> entity.status
             *     | Some dir -> Moving (dir, 0.0, 0.3)
             *   else entity.status
             * in
             *
             * let target =
             *   let find_next ~restart ~self target =
             *     Entities.fold_while
             *       (fun (old_found, next) index _ ->
             *         if Entities.(equal index self) then
             *           ((old_found, next), `Continue)
             *         else if Entities.(equal index target) then
             *           ((true, next), `Continue)
             *         else if old_found then ((old_found, Some index), `Stop)
             *         else
             *           match next with
             *           | Some _ -> ((old_found, next), `Continue)
             *           | None -> ((old_found, Some index), `Continue))
             *       (restart, None) entities
             *   in
             *   if entity.is_player && is_key_pressed Key.Tab then
             *     match entity.target with
             *     | Some target ->
             *         let _, next = find_next ~self:index ~restart:false target in
             *         next
             *     | None ->
             *         let _, next = find_next ~self:index ~restart:true index in
             *         next
             *   else entity.target
             * in *)
            let vec_pos, entity = move dt layout entity in

            (* let should_cast =
             *   if entity.is_player then fun s -> keymap s.typ
             *   else fun _ -> List.mem (Is_Aggro true) entity.comps
             * in
             *
             * let status, evs, skills =
             *   handle_skills dt should_cast entity.skills entity.status
             *     ( match entity.target with
             *     | None -> None
             *     | Some target -> (
             *         match Entities.get entities target with
             *         | None -> None
             *         | Some target_ent ->
             *             Some
             *               ( target,
             *                 pseudo_dist entity.position target_ent.position ) )
             *     )
             * in
             *
             * let comps =
             *   List.filter_map
             *     (function
             *       | Dodging 0.0 -> None
             *       | Dodging t -> Some (Dodging (t -. dt |> Float.max 0.0))
             *       | other -> Some other)
             *     entity.comps
             * in
             *
             * let entity = { entity with skills; status; comps; target } in
             *
             * let events =
             *   List.fold_left
             *     (fun events (effect, target) ->
             *       { effect; caster = index; target } :: events)
             *     events evs
             * in *)

            (* TODO handle evs and stuff *)
            (((vec_pos, entity.texture) :: positions, events), entity))
          ([], []) entities
      in

      (* (\* apply events *\)
       * let entities =
       *   List.fold_left
       *     (fun entities { effect; caster = index_caster; target } ->
       *       (\* TODO entity creation must be handled here *\)
       *       match Entities.get entities index_caster with
       *       | None -> entities
       *       | Some caster ->
       *           ( match target with
       *           | None ->
       *               let ent_caster, _ =
       *                 handle_ev ~caster ~target:caster effect
       *               in
       *               Entities.set entities index_caster ent_caster
       *           | Some index_target -> (
       *               match Entities.get entities index_target with
       *               | None -> ()
       *               | Some target ->
       *                   let ent_caster, ent_target =
       *                     handle_ev ~caster ~target effect
       *                   in
       *                   Entities.set entities index_caster ent_caster;
       *                   Entities.set entities index_target ent_target ) );
       *           entities)
       *     entities events
       * in
       *
       * (\* update ui  *\)
       * let rec up_ui ui =
       *   match Entities.get entities ui.player with
       *   | Some player -> (
       *       let ui = update_ui ui player in
       *       match (player.target, ui.ui) with
       *       | Some target, Some target_ui ->
       *           { ui with ui = Some (up_ui { target_ui with player = target }) }
       *       | Some target, None ->
       *           {
       *             ui with
       *             ui = Some (up_ui (create_ui ~innerc:Color.red target));
       *           }
       *       | None, None | None, Some _ -> { ui with ui = None } )
       *   | None -> ui
       * in
       *
       * let ui = up_ui ui in *)
      let rec gameplay_loop queue =
        let _, move =
          match Queue.head queue with
          | Some (Entity ent) -> (
              match Entities.get entities ent with
              | Some ent -> tick_ent ent entities
              | None -> ([], `Drop) )
          | Some Turn ->
              Printf.printf "next turn\n%!";
              ([], `Turn 10)
          | None -> ([], `Not_yet)
        in

        match move with
        | `Npc duration | `Turn duration ->
            gameplay_loop (Queue.advance_head queue (`Enqueue duration))
        | `Player duration -> Queue.advance_head queue (`Enqueue duration)
        | `Not_yet -> queue
        | `Drop -> gameplay_loop (Queue.advance_head queue `Drop)
      in
      let turn_queue = gameplay_loop turn_queue in

      (* should this be a loop? *)
      (* let turn_queue =
       *   match Turn_queue.take turn_queue with
       *   | Some (q, pent) -> (
       *       match pent with
       *       | prio, Entity ent ->
       *           let a, b = Entities.to_hash ent in
       *           Printf.printf "next is %i, %i\n%!" a b;
       *           let q = Turn_queue.add q (prio + 10, Entity ent) in
       *           q
       *       | prio, Turn ->
       *           Printf.printf "next turn\n%!";
       *           let q = Turn_queue.add q (prio + 10, Turn) in
       *           q )
       *   | None ->
       *       Printf.printf "queue empty\n%!";
       *       turn_queue
       * in *)
      begin_drawing ();

      clear_background Color.raywhite;

      (* draw grid *)
      let ofs = Vector2.create 3.0 3.0 in
      let ofs2 = Vector2.scale ofs 2.0 in
      let border_size = Vector2.subtract Tile.(layout.size) ofs2 in
      let inner_size = Vector2.subtract border_size ofs2 in

      for y = -2 to 2 do
        for x = -2 to 2 do
          let vec = Vector2.add (Tile.vec_of_tl { x; y } layout) ofs in
          draw_rectangle_v vec border_size Color.lightgray;
          draw_rectangle_v (Vector2.add vec ofs) inner_size Color.raywhite
        done
      done;

      List.iter
        (fun (position, texture) ->
          (* make sure the image is contained in the tile *)
          (* works as long as size in layout is same x and y *)
          let scale =
            Vector2.x inner_size /. Float.of_int (Texture2D.width texture)
          in
          draw_texture_ex texture
            (Vector2.add ofs2 position)
            0.0 scale Color.white)
        positions;

      draw_ui ui;

      end_drawing ();

      loop entities layout ui turn_queue

let () =
  let layout =
    {
      Tile.size = Raylib.Vector2.create 10.0 10.0;
      origin = Raylib.Vector2.create 200.0 200.0;
    }
  in
  let p1 = Tile.from_px 200.0 201.0 layout in

  assert (p1.x = 0);
  assert (p1.y = 0);

  let arr = [| 0; 3 |] in
  arr.(0) <- 14;
  let arr = Array.append arr [| 67 |] in
  assert (arr.(0) = 14)

let () =
  let entities, layout, ui, turn_queue = setup () in
  loop entities layout ui turn_queue
