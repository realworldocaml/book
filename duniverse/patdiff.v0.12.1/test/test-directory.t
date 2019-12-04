Test recursive diff of directories.

  $ mkdir prev
  $ mkdir prev/foo
  $ echo . > prev/this-goes-away
  $ echo prev > prev/this-changes

  $ mkdir next
  $ echo . > next/foo
  $ echo . > next/this-appears
  $ echo next > next/this-changes


  $ patdiff.exe -default prev next | visible_colors
  Only in prev: this-goes-away (glob)
  (fg:red)------ (+bold)prev/this-goes-away
  (fg:green)++++++ (+bold)/dev/null
  (fg:black)@|(+bold)-1,1 +1,0(off) ============================================================
  (fg:black bg:red)-|(fg:red).
  Only in next: this-appears (glob)
  (fg:red)------ (+bold)/dev/null
  (fg:green)++++++ (+bold)next/this-appears
  (fg:black)@|(+bold)-1,0 +1,1(off) ============================================================
  (fg:black bg:green)+|(fg:green).
  Files prev/foo and next/foo are not the same type (glob)
  (fg:red)------ (+bold)prev/this-changes
  (fg:green)++++++ (+bold)next/this-changes
  (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
  (fg:black bg:red)-|(fg:red)prev
  (fg:black bg:green)+|(fg:green)next
