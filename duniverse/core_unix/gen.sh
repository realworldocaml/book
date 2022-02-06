#!/usr/bin/env bash

renaming_lib() {
    lib_name=$1
    redirecting_lib_name=core_unix_$1
    module_name="${lib_name^}"

    original_public_name=core.$lib_name
    case $lib_name in
        uuid_unix)
            original_public_name=core.uuid
            ;;
    esac

    rm -r "$lib_name"
    mkdir "$lib_name"
    mkdir "$lib_name"/src
    cat > "$lib_name"/src/"$redirecting_lib_name".ml <<EOF
include $module_name
EOF
    cat > "$lib_name"/src/"$redirecting_lib_name".mli <<EOF
include module type of struct include $module_name end
EOF
    public_name=core_unix.$lib_name
    if [ "$lib_name" = "core_unix" ]; then
       public_name=core_unix
    fi
    cat > "$lib_name"/src/dune <<EOF
(library (name $redirecting_lib_name) (public_name $public_name)
  (libraries $original_public_name) (preprocess no_preprocessing))
EOF
}

forward_to_core_module() {
    lib_name=$1
    core_module=$2
    rm -r "$lib_name"
    mkdir "$lib_name"
    mkdir "$lib_name"/src

    cat > "$lib_name"/src/"$lib_name".ml <<EOF
include Core.$core_module
EOF
    cat > "$lib_name"/src/"$lib_name".mli <<EOF
include module type of struct include Core.$core_module end
EOF
    public_name=core_unix.$lib_name
    if [ "$lib_name" = "core_unix" ]; then
       public_name=core_unix
    fi
    cat > "$lib_name"/src/dune <<EOF
(library (name $lib_name) (public_name $public_name)
  (libraries core) (preprocess no_preprocessing))
EOF
}

# library renames core.foo -> core_unix.foo
renaming_lib bigbuffer_blocking
renaming_lib bigstring_unix
renaming_lib uuid_unix
renaming_lib syslog
renaming_lib squeue
renaming_lib iobuf_unix
renaming_lib nano_mutex
renaming_lib process_env
renaming_lib daemon
renaming_lib error_checking_mutex
renaming_lib linux_ext
renaming_lib lock_file_blocking
renaming_lib time_stamp_counter

# module extractions
forward_to_core_module command_unix Command
forward_to_core_module date_unix Date
forward_to_core_module filename_unix Filename
forward_to_core_module signal_unix Signal
forward_to_core_module sys_unix Sys
forward_to_core_module core_thread Thread
forward_to_core_module time_unix Time
forward_to_core_module time_ns_unix Time_ns
forward_to_core_module core_unix Unix
forward_to_core_module time_interface Time_common

# interval_lib: made manually

# pre-existing libraries that no longer exist:
#(core_top core.top)
#(core core)

# new libs not present in v0.14:
#(command_test_helpers core_unix.command_test_helpers)
#(command_test_helpers_test core_unix.command_test_helpers_test)
#(interval_lib core_unix.interval_lib)
#(ocaml_c_utils core_unix.ocaml_c_utils)
#(profunctor core_unix.profunctor)
#(unix_pseudo_terminal core_unix.unix_pseudo_terminal)
