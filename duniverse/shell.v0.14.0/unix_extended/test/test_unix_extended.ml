open! Core
open! Import
open! Unix_extended

let%expect_test "[Mount_entry.parse_line]" =
  List.iter
    [ "/dev/mapper/vg01-root / ext4 defaults 0 0"
    ; "/dev/mapper/vg01-swap swap swap defaults"
    ; "  extra_whitespace\t\t\t /extra_whitespace \t\t ext4 rw 0 \t "
    ; "weird_comments /weird_comments ext4 defaults 0 0 # comment # another"
    ; "embedded_space /embedded\\040space ext4 defaults"
    ; "# leading comment"
    ; " # space then comment"
    ; "    "
    ; "LABEL=boot /boot ext4 defaults 0"
    ; "missing_escape \\999 xfs defaults" (* Errors follow *)
    ; "almost enough fields"
    ; "even fewer"
    ; "just_one"
    ; "\\"
    ]
    ~f:(fun input ->
      print_s
        [%message
          ""
            (input : string)
            ~output:(Mount_entry.parse_line input : Mount_entry.t option Or_error.t)]);
  [%expect
    {|
    ((input "/dev/mapper/vg01-root / ext4 defaults 0 0")
     (output (
       Ok ((
         (fsname    /dev/mapper/vg01-root)
         (directory /)
         (fstype    ext4)
         (options   defaults))))))
    ((input "/dev/mapper/vg01-swap swap swap defaults")
     (output (
       Ok ((
         (fsname    /dev/mapper/vg01-swap)
         (directory swap)
         (fstype    swap)
         (options   defaults))))))
    ((input "  extra_whitespace\t\t\t /extra_whitespace \t\t ext4 rw 0 \t ")
     (output (
       Ok ((
         (fsname    extra_whitespace)
         (directory /extra_whitespace)
         (fstype    ext4)
         (options   rw))))))
    ((input
      "weird_comments /weird_comments ext4 defaults 0 0 # comment # another")
     (output (
       Ok ((
         (fsname    weird_comments)
         (directory /weird_comments)
         (fstype    ext4)
         (options   defaults))))))
    ((input "embedded_space /embedded\\040space ext4 defaults")
     (output (
       Ok ((
         (fsname    embedded_space)
         (directory "/embedded space")
         (fstype    ext4)
         (options   defaults))))))
    ((input "# leading comment") (output (Ok ())))
    ((input " # space then comment") (output (Ok ())))
    ((input "    ") (output (Ok ())))
    ((input "LABEL=boot /boot ext4 defaults 0")
     (output (
       Ok ((
         (fsname    LABEL=boot)
         (directory /boot)
         (fstype    ext4)
         (options   defaults))))))
    ((input "missing_escape \\999 xfs defaults")
     (output (
       Ok ((
         (fsname    missing_escape)
         (directory "\\999")
         (fstype    xfs)
         (options   defaults))))))
    ((input "almost enough fields")
     (output (Error ("wrong number of fields" "almost enough fields"))))
    ((input "even fewer")
     (output (Error ("wrong number of fields" "even fewer"))))
    ((input just_one) (output (Error ("wrong number of fields" just_one))))
    ((input \) (output (Error ("wrong number of fields" \)))) |}]
;;

let%expect_test "[Mount_entry.visible_filesystem]" =
  let mount_entries =
    List.filter_map
      ~f:(Fn.compose Or_error.ok_exn Mount_entry.parse_line)
      [ "# a comment"
      ; "/dev/mapper/vg01-root / ext4 rw 0 0"
      ; "/dev/mapper/vg01-var /var ext4 rw 0 0"
      ; "/dev/mapper/vg01-tmp /tmp ext4 rw 0 0"
      ; "/dev/mapper/vg01-swap swap swap rw 0 0"
      ; "some-server1:/some/mount/point /mnt/something/else nfs \
         some-option,ro,vers=3,hard,intr 0 0"
      ]
  in
  let visible_filesystem = Mount_entry.visible_filesystem mount_entries in
  print_s ([%sexp_of: Mount_entry.t String.Map.t] visible_filesystem);
  [%expect
    {|
    ((/ (
       (fsname    /dev/mapper/vg01-root)
       (directory /)
       (fstype    ext4)
       (options   rw)))
     (/mnt/something/else (
       (fsname    some-server1:/some/mount/point)
       (directory /mnt/something/else)
       (fstype    nfs)
       (options   some-option,ro,vers=3,hard,intr)))
     (/tmp (
       (fsname    /dev/mapper/vg01-tmp)
       (directory /tmp)
       (fstype    ext4)
       (options   rw)))
     (/var (
       (fsname    /dev/mapper/vg01-var)
       (directory /var)
       (fstype    ext4)
       (options   rw))))
|}]
;;
