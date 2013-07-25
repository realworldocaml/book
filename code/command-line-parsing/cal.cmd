function _jsautocom_96173 {
  export COMP_CWORD
  COMP_WORDS[0]=./cal_add_sub_days.native
  COMPREPLY=($("${COMP_WORDS[@]}"))
}
complete -F _jsautocom_96173 ./cal_add_sub_days.native
