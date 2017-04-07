
#load "graphics.cma"
#load "unix.cma"

open Random;;

Random.self_init ();;

type cell = {
		mutable mv_down : bool; mutable mv_right : bool;
		mutable used : bool; mutable pr : int * int; }

let mk_clear_maze hg wd = 
	Array.init hg (fun _ -> Array.init wd (fun _ -> { 
			mv_down = false; mv_right = false;
			used = false; pr = (0, 0) }
		));;

let maze_hg mz = Array.length mz;;

let maze_wd mz = Array.length mz.(0);;

let check mz y x =
	x >= 0 && x < maze_wd mz &&
	y >= 0 && y < maze_hg mz && 
	mz.(y).(x).used == false

let rec dfs mz y x pry prx = 
	let ms = [| (-1, 0); (1, 0); (0, -1); (0, 1) |] and z = ref (0, 0) in 
	if check mz y x then 
	begin 
		if pry != y then 
			if pry < y then mz.(pry).(x).mv_down <- true
			else mz.(y).(x).mv_down <- true
		else 
			if prx < x then mz.(y).(prx).mv_right <- true
			else mz.(y).(x).mv_right <- true;
		mz.(y).(x).used <- true;
		mz.(y).(x).pr <- (pry, prx);
		for i = 0 to 3 do let t = Random.int 4 in
			z := ms.(i);
			ms.(i) <- ms.(t);
			ms.(t) <- !z;
		done;
		for i = 0 to 3 do let (dy, dx) = ms.(i) in
			dfs mz (y + dy) (x + dx) y x;
		done;
	end;;

let console_draw printer hg wd = 
	begin
		for i = 0 to 2 * hg do
			for j = 0 to 2 * wd do Printf.printf "%c" printer.(i).(j) done;
			Printf.printf "\n";
		done;
	end;;

let graph_draw printer hg wd= 
	let w = wd * 2 + 1 and h = hg * 2 + 1 in
	begin
		Graphics.open_graph "";
		Graphics.set_window_title "MezesGen-OCaml";
		Graphics.resize_window (w*10) (h * 10);
		Graphics.set_color Graphics.black;
		for i = 0 to h - 1 do for j = 0 to w - 1 do
			if printer.(i).(j) == '#' then Graphics.fill_rect (j * 10) (i * 10) 10 10;
		done done;
		Graphics.wait_next_event [Graphics.Key_pressed];
	end;;

let main height width =
	let maze = mk_clear_maze height width in
	let printer = Array.init (2 * height + 1) (fun _ -> Array.make (2 * width + 1) '#') in 
	begin
		dfs maze 0 0 0 0;
		for i = 0 to maze_hg maze - 1 do for j = 0 to maze_wd maze - 1 do
			printer.(1 + i * 2).(1 + j * 2) <- '.';
			printer.(1 + i * 2 + 1).(1 + j * 2) <- 
				if maze.(i).(j).mv_down then '.' else '#';
			printer.(1 + i * 2).(1 + j * 2 + 1) <- 
				if maze.(i).(j).mv_right then '.' else '#';
		done done;
		graph_draw printer height width;
	end;;

main 30 60;;
