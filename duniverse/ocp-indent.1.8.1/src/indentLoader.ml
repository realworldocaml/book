(**************************************************************************)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

module SS = Set.Make(String)

let loaded = ref SS.empty

let predicates =
  if Dynlink.is_native
  then [ "plugin" ; "native" ]
  else [ "plugin" ; "byte" ]

let dynlink debug s =
  if debug then Format.eprintf "loading archive %s..@." s;
  try
    Dynlink.loadfile s
  with exc ->
    Format.eprintf "Error while linking %s : %s@." s (Printexc.to_string exc);
    raise exc

let load_pkg debug pkg =
  if not (SS.mem pkg !loaded) then
    begin
      let d = Findlib.package_directory pkg in
      let archive =
        try Findlib.package_property predicates pkg "archive"
        with
          Not_found -> ""
      in
      let archives = Util.string_split ' ' archive in
      List.iter (fun arch ->
          if arch <> ""
          then
            let arch' = Findlib.resolve_path ~base:d arch in
            dynlink debug arch')
        archives;
      loaded:=SS.add pkg !loaded
    end

let rec partition mods pkgs = function
  | [] -> List.rev pkgs, List.rev mods
  | `Mod m :: rest -> partition (m::mods) pkgs rest
  | `Pkg p :: rest -> partition mods (p::pkgs) rest

let load ?(debug=false) l =
  let pkglist,mods = partition [] [] l in
  List.iter (dynlink debug) mods;
  let eff_pkglist =
    Findlib.package_deep_ancestors predicates pkglist in
  List.iter (load_pkg debug) eff_pkglist
