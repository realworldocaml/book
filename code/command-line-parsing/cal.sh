function _jsautocom_622 {
  export COMP_CWORD
  COMP_WORDS[0]=./cal_add_sub_days.native
  COMPREPLY=($("${COMP_WORDS[@]}"))
}
complete -F _jsautocom_622 ./cal_add_sub_days.native
