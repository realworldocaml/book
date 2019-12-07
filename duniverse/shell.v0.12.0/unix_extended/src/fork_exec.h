/* value extended_ml_create_process */
extern CAMLprim value extended_ml_spawn
(
 value v_stdin, /* Fd to connect to the forked stdin... */
 value v_stdout,
 value v_stderr,
 value v_working_dir, /* A directory we want to chdir too. [String option] */
 value v_setuid, /* setuid on the fork side [int option] */
 value v_setgid, /* setgid on the fork side [int option] */
 value v_env, /* The Environment to set for execve. pass None to call an
                 execv instead. [string array option]*/
 value v_prog, /* Program name [string] */
 value v_args /* Full list of args passed to executable [string array] */
 );

extern CAMLprim value extended_ml_spawn_bc(value *argv, int argn);
