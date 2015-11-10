open Graphics
module Random = Core_kernel.Core_random

type point =
  {
    mutable x : float;
    mutable y : float;
  }

type boid =
  {
    location : point;
    velocity : point;
  }

let magnitude point =
  sqrt (point.x ** 2. +. point.y ** 2.)

let add a b =
  { x = a.x +. b.x; y = a.y +. b.y }

let sub a b =
  { x = a.x -. b.x; y = a.y -. b.y }

let scale k a =
  { x = a.x *. k; y = a.y *. k }

let div k a =
  { x = a.x /. k; y = a.y /. k }

let dist a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  magnitude { x = dx; y = dy; }

let cap_magnitude cap point =
  let mag = magnitude point in  
  if mag <= cap then point
  else let diff = mag -. cap in
  let scaler = 1. -. (diff /. mag) in
  { x = scaler *. point.x; y = scaler *. point.y; }

let random_point bounds =
  let x = Random.float bounds.x in
  let y = Random.float bounds.y in
  { x = x; y = y; }

let random_boid bounds max_vel =
  let loc = random_point bounds in
  let vel = random_point { x = max_vel; y = max_vel; } in
  { location = loc; velocity = cap_magnitude max_vel vel; }

let random_flock bounds max_vel num_boids =
  let new_boid i =
    random_boid bounds max_vel in
  Array.init num_boids new_boid

let move bounds { location = loc; velocity = vel; } =
  let x = loc.x +. vel.x in
  let y = loc.y +. vel.y in
  let new_loc = { x = x; y = y; } in
  { location = new_loc; velocity = vel; }

let bounce bounds { location = loc; velocity = vel; } =
  let x = if (loc.x < 0. && vel.x < 0.) || (loc.x > bounds.x && vel.x > 0.) then
    vel.x *. -1. else vel.x in
  let y = if (loc.y < 0. && vel.y < 0.) || (loc.y > bounds.y && vel.y > 0.) then
    vel.y *. -1. else vel.y in
  let new_vel = { x = x; y = y; } in
  { location = loc; velocity = new_vel; }

let perceived_centres flock =
  let add_boid boid point =
    add point boid.location in
  let sum = Array.fold_right add_boid flock { x = 0.; y = 0.; } in
  let perceived_centre sum boid =
    let perceived_sum = sub sum boid.location in
    div (float (Array.length flock) -. 1.) perceived_sum in
  Array.map (perceived_centre sum) flock

let adjustment_to_centre centre_factor centre { location = loc; velocity = vel; } =
  let to_centre = sub centre loc in
  let factor = centre_factor in
  div factor to_centre

let move_flock_to_centre centre_factor flock =
  let centres = perceived_centres flock in
  let move i boid =
    adjustment_to_centre centre_factor centres.(i) boid in
  Array.mapi move flock

let repel_from_each_other min_dist flock =
  let repel flock i { location = loc; velocity = vel; } =
    let r = ref { x = 0.; y = 0. } in
    for j = 0 to Array.length flock - 1 do
      if j != i then let jloc = flock.(j).location in
      if dist jloc loc < min_dist then r := sub !r (sub jloc loc)
    done;
    !r in
  Array.mapi (repel flock) flock

let perceived_velocities flock =
  let sum_vel boid point =
    add point boid.velocity in
  let sum = Array.fold_right sum_vel flock { x = 0.; y = 0.; } in
  let perceived_velocity sum boid =
    let perceived_sum = sub sum boid.velocity in
    div (float (Array.length flock) -. 1.) perceived_sum in
  Array.map (perceived_velocity sum) flock

let match_flock_velocity vel_factor flock =
  let velocities = perceived_velocities flock in
  let adjust i { location = loc; velocity = vel; } =
    div vel_factor (sub velocities.(i) vel) in
  Array.mapi adjust flock

let rec sum_rules flock rules =
  let add_rule rule i { location = loc; velocity = vel; } =
    let adjust = rule.(i) in
    let new_vel = add vel adjust in
    { location = loc; velocity = new_vel; } in
  match rules with
  | [] -> flock
  | hd :: tl ->
      let flock = Array.mapi (add_rule hd) flock in
      sum_rules flock tl

let cap_vel max_vel { location = loc; velocity = vel; } =
  { location = loc; velocity = cap_magnitude max_vel vel; }

let tick bounds min_dist max_vel centre_factor vel_factor flock =
  let rule1 = move_flock_to_centre centre_factor flock in
  let rule2 = repel_from_each_other min_dist flock in
  let rule3 = match_flock_velocity vel_factor flock in
  let flock = sum_rules flock [rule1; rule2; rule3] in
  let flock = Array.map (cap_vel max_vel) flock in
  let flock = Array.map (bounce bounds) flock in
  Array.map (move bounds) flock

let draw_boid rad boid =
  let loc = boid.location in
  fill_circle (int_of_float loc.x) (int_of_float loc.y) rad

let draw flock rad =
  clear_graph ();
  Array.iter (draw_boid rad) flock

let init () =
  Random.self_init ();
  open_graph " 800x800";
  set_color black

let rec delay sleep =
  try
    Thread.delay sleep
  with Unix.Unix_error (Unix.EINTR, _, _) -> delay sleep

let () =
  let rad = 5 in
  let bounds = { x = 800.; y = 800.; } in
  let max_vel = 5. in
  let min_dist = 30. in
  let center_factor = 1000. in
  let vel_factor = 16. in
  let sleep = 0.05 in
  let num_boids = 40 in
  let flock = ref (random_flock bounds max_vel num_boids) in
  init ();
  while true do
    draw (!flock) rad;
    flock := tick bounds min_dist max_vel center_factor vel_factor (!flock);
    delay sleep;
    print_string "tick";
    print_newline ()
  done

