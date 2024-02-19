(** Representation and parsing of Dune files *)

open Import

module Lint : sig
  type t = Preprocess.Without_instrumentation.t Preprocess.Per_module.t

  val no_lint : t
end

type for_ =
  | Executable
  | Library of Wrapped.t option

module Lib_deps : sig
  type nonrec t = Lib_dep.t list

  val of_pps : Lib_name.t list -> t

  val decode : for_ -> t Dune_lang.Decoder.t
end

module Buildable : sig
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; empty_module_interface_if_absent : bool
    ; libraries : Lib_dep.t list
    ; foreign_archives : (Loc.t * Foreign.Archive.t) list
    ; extra_objects : Foreign.Objects.t
    ; foreign_stubs : Foreign.Stubs.t list
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; lint : Lint.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.In_buildable.t
    ; allow_overlapping_dependencies : bool
    ; ctypes : Ctypes_stanza.t option
    ; root_module : (Loc.t * Module_name.t) option
    }

  (** Check if the buildable has any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** Check if the buildable has any c++ foreign stubs. *)
  val has_foreign_cxx : t -> bool

  val has_mode_dependent_foreign_stubs : t -> bool
end

module Public_lib : sig
  type t

  (** Subdirectory inside the installation directory *)
  val sub_dir : t -> string option

  val loc : t -> Loc.t

  (** Full public name *)
  val name : t -> Lib_name.t

  (** Package it is part of *)
  val package : t -> Package.t

  val make :
       allow_deprecated_names:bool
    -> Dune_project.t
    -> Loc.t * Lib_name.t
    -> (t, User_message.t) result
end

module Mode_conf : sig
  type t =
    | Byte
    | Native
    | Best  (** [Native] if available and [Byte] if not *)

  val decode : t Dune_lang.Decoder.t

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  module Kind : sig
    type t =
      | Inherited
      | Requested of Loc.t
  end

  module Map : sig
    type nonrec 'a t =
      { byte : 'a
      ; native : 'a
      ; best : 'a
      }
  end

  type mode_conf := t

  module Set : sig
    type nonrec t = Kind.t option Map.t

    val of_list : (mode_conf * Kind.t) list -> t

    val decode : t Dune_lang.Decoder.t

    module Details : sig
      type t = Kind.t option
    end

    val eval_detailed : t -> has_native:bool -> Details.t Mode.Dict.t

    val eval : t -> has_native:bool -> Mode.Dict.Set.t
  end

  module Lib : sig
    type t =
      | Ocaml of mode_conf
      | Melange

    val to_dyn : t -> Dyn.t

    module Map : sig
      type nonrec 'a t =
        { ocaml : 'a Map.t
        ; melange : 'a
        }
    end

    module Set : sig
      type mode_conf := t

      type nonrec t = Kind.t option Map.t

      val of_list : (mode_conf * Kind.t) list -> t

      val decode : t Dune_lang.Decoder.t

      module Details : sig
        type t = Kind.t option
      end

      val eval_detailed : t -> has_native:bool -> Details.t Lib_mode.Map.t

      val eval : t -> has_native:bool -> Lib_mode.Map.Set.t
    end
  end
end

module Library : sig
  type visibility =
    | Public of Public_lib.t
    | Private of Package.t option

  type t =
    { name : Loc.t * Lib_name.Local.t
    ; visibility : visibility
    ; synopsis : string option
    ; install_c_headers : string list
    ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
    ; modes : Mode_conf.Lib.Set.t
    ; kind : Lib_kind.t
          (* TODO: It may be worth remaming [c_library_flags] to
             [link_time_flags_for_c_compiler] and [library_flags] to
             [link_time_flags_for_ocaml_compiler], both here and in the Dune
             language, to make it easier to understand the purpose of various
             flags. Also we could add [c_library_flags] to [Foreign.Stubs.t]. *)
    ; library_flags : Ordered_set_lang.Unexpanded.t
    ; c_library_flags : Ordered_set_lang.Unexpanded.t
    ; virtual_deps : (Loc.t * Lib_name.t) list
    ; wrapped : Wrapped.t Lib_info.Inherited.t
    ; optional : bool
    ; buildable : Buildable.t
    ; dynlink : Dynlink_supported.t
    ; project : Dune_project.t
    ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
    ; dune_version : Dune_lang.Syntax.Version.t
    ; virtual_modules : Ordered_set_lang.t option
    ; implements : (Loc.t * Lib_name.t) option
    ; default_implementation : (Loc.t * Lib_name.t) option
    ; private_modules : Ordered_set_lang.t option
    ; stdlib : Ocaml_stdlib.t option
    ; special_builtin_support : Lib_info.Special_builtin_support.t option
    ; enabled_if : Blang.t
    ; instrumentation_backend : (Loc.t * Lib_name.t) option
    }

  val sub_dir : t -> string option

  val package : t -> Package.t option

  (** Check if the library has any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** Check if the library has any c++ foreign stubs. *)
  val has_foreign_cxx : t -> bool

  (** The foreign stubs archive. *)
  val stubs_archive : t -> Foreign.Archive.t option

  (** The list of foreign archives. *)
  val foreign_archives : t -> Foreign.Archive.t list

  (** The [lib*.a] files of all foreign archives, including foreign stubs. [dir]
      is the directory the library is declared in. Only files relevant to the
      [for_mode] selection will be returned. *)
  val foreign_lib_files :
       t
    -> dir:Path.Build.t
    -> ext_lib:string
    -> for_mode:Mode.Select.t
    -> Path.Build.t list

  (** The path to a library archive. [dir] is the directory the library is
      declared in. *)
  val archive : t -> dir:Path.Build.t -> ext:string -> Path.Build.t

  val best_name : t -> Lib_name.t

  val is_virtual : t -> bool

  val is_impl : t -> bool

  val obj_dir : dir:Path.Build.t -> t -> Path.Build.t Obj_dir.t

  val main_module_name : t -> Lib_info.Main_module_name.t

  val to_lib_info :
    t -> dir:Path.Build.t -> lib_config:Lib_config.t -> Lib_info.local Memo.t
end

module Plugin : sig
  type t =
    { package : Package.t
    ; name : Package.Name.t
    ; libraries : (Loc.t * Lib_name.t) list
    ; site : Loc.t * (Package.Name.t * Section.Site.t)
    ; optional : bool
    }
end

module Install_conf : sig
  module File_entry : sig
    type t

    val to_file_bindings_unexpanded :
         t list
      -> expand_str:(String_with_vars.t -> string Memo.t)
      -> dir:Path.Build.t
      -> File_binding.Unexpanded.t list Memo.t
  end

  module Dir_entry : sig
    type t
  end

  type t =
    { section : Install.Section_with_site.t
    ; files : File_entry.t list
    ; dirs : Dir_entry.t list
    ; package : Package.t
    ; enabled_if : Blang.t
    }

  val expand_files :
       t
    -> expand_str:(String_with_vars.t -> string Memo.t)
    -> dir:Path.Build.t
    -> File_binding.Expanded.t list Memo.t

  val expand_dirs :
       t
    -> expand_str:(String_with_vars.t -> string Memo.t)
    -> dir:Path.Build.t
    -> File_binding.Expanded.t list Memo.t
end

module Executables : sig
  module Link_mode : sig
    type t =
      | Byte_complete
      | Other of
          { mode : Mode_conf.t
          ; kind : Binary_kind.t
          }

    include Dune_lang.Conv.S with type t := t

    val exe : t

    val object_ : t

    val shared_object : t

    val byte : t

    val native : t

    val js : t

    val compare : t -> t -> Ordering.t

    val to_dyn : t -> Dyn.t

    val extension : t -> loc:Loc.t -> ext_obj:string -> ext_dll:string -> string

    module Map : Map.S with type key = t
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Link_flags.Spec.t
    ; link_deps : Dep_conf.t list
    ; modes : Loc.t Link_mode.Map.t
    ; optional : bool
    ; buildable : Buildable.t
    ; package : Package.t option
    ; promote : Rule.Promote.t option
    ; install_conf : Install_conf.t option
    ; embed_in_plugin_libraries : (Loc.t * Lib_name.t) list
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    ; bootstrap_info : string option
    ; enabled_if : Blang.t
    ; dune_version : Dune_lang.Syntax.Version.t
    }

  (** Check if the executables have any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** Check if the executables have any c++ foreign stubs. *)
  val has_foreign_cxx : t -> bool

  val obj_dir : t -> dir:Path.Build.t -> Path.Build.t Obj_dir.t
end

module Copy_files : sig
  type t =
    { add_line_directive : bool
    ; alias : Alias.Name.t option
    ; mode : Rule.Mode.t
    ; enabled_if : Blang.t
    ; files : String_with_vars.t
    ; syntax_version : Dune_lang.Syntax.Version.t
    }
end

module Rule : sig
  type t =
    { targets : String_with_vars.t Targets_spec.t
    ; deps : Dep_conf.t Bindings.t
    ; action : Loc.t * Dune_lang.Action.t
    ; mode : Rule.Mode.t
    ; patch_back_source_tree : bool
    ; locks : Locks.t
    ; loc : Loc.t
    ; enabled_if : Blang.t
    ; aliases : Alias.Name.t list
    ; package : Package.t option
    }
end

module Alias_conf : sig
  type t =
    { name : Alias.Name.t
    ; deps : Dep_conf.t Bindings.t
    ; action : (Loc.t * Dune_lang.Action.t) option
    ; locks : Locks.t
    ; package : Package.t option
    ; enabled_if : Blang.t
    ; loc : Loc.t
    }
end

module Documentation : sig
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }
end

module Tests : sig
  type t =
    { exes : Executables.t
    ; locks : Locks.t
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; action : Dune_lang.Action.t option
    }
end

module Toplevel : sig
  type t =
    { name : string
    ; libraries : (Loc.t * Lib_name.t) list
    ; loc : Loc.t
    ; pps : Preprocess.Without_instrumentation.t Preprocess.t
    }
end

module Include_subdirs : sig
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification
end

(** The purpose of [Library_redirect] stanza is to create a redirection from an
    [old_name] to a [new_public_name].

    This is used in two cases:

    - When a library changes its public name, a redirection is created for
      backwards compatibility with the code using its old name.
      (deprecated_library_name stanza in dune files)

    - When hiding public libraries with [--only-packages] (or [-p]), we use this
      stanza to make sure that their project-local names remain in scope. *)
module Library_redirect : sig
  type 'old_name t = private
    { project : Dune_project.t
    ; loc : Loc.t
    ; old_name : 'old_name
    ; new_public_name : Loc.t * Lib_name.t
    }

  module Local : sig
    type nonrec t = (Loc.t * Lib_name.Local.t) t

    val of_private_lib : Library.t -> t option
  end
end

module Deprecated_library_name : sig
  module Old_name : sig
    type deprecation =
      | Not_deprecated
      | Deprecated of { deprecated_package : Package.Name.t }

    type t = Public_lib.t * deprecation
  end

  type t = Old_name.t Library_redirect.t

  val old_public_name : t -> Lib_name.t
end

(** Stanza which generate a module for getting information from dune *)
module Generate_sites_module : sig
  type t =
    { loc : Loc.t
    ; module_ : Module_name.t  (** name of the module to generate *)
    ; sourceroot : bool  (** should the sourceroot of the project be provided *)
    ; relocatable : bool
          (** should the fact that the installation use the relocatable mode *)
    ; sites : (Loc.t * Package.Name.t) list
          (** list of the sites whose location should be given *)
    ; plugins : (Loc.t * (Package.Name.t * (Loc.t * Section.Site.t))) list
          (** list of the sites for which a plugin system must be provided *)
    }
end

type Stanza.t +=
  | Library of Library.t
  | Foreign_library of Foreign.Library.t
  | Executables of Executables.t
  | Rule of Rule.t
  | Install of Install_conf.t
  | Alias of Alias_conf.t
  | Copy_files of Copy_files.t
  | Documentation of Documentation.t
  | Tests of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t
  | Toplevel of Toplevel.t
  | Library_redirect of Library_redirect.Local.t
  | Deprecated_library_name of Deprecated_library_name.t
  | Cram of Cram_stanza.t
  | Generate_sites_module of Generate_sites_module.t
  | Plugin of Plugin.t
  | Melange_emit of Melange_stanzas.Emit.t

val stanza_package : Stanza.t -> Package.t option

(** [of_ast project ast] is the list of [Stanza.t]s derived from decoding the
    [ast] according to the syntax given by [kind] in the context of the
    [project] *)
val of_ast : Dune_project.t -> Dune_lang.Ast.t -> Stanza.t list

(** A fully evaluated dune file *)
type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanza.t list
  }

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

val parse :
     Dune_lang.Ast.t list
  -> dir:Path.Source.t
  -> file:Path.Source.t
  -> project:Dune_project.t
  -> t Memo.t

val fold_stanzas :
  t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

module Memo_fold : sig
  val fold_stanzas :
       t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end
