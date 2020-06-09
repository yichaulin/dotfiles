reset_prompt() {
  local prefix=${AWS_PROFILE:-default}
  PS1="%{$fg_bold[yellow]%}[${prefix}] %{$reset_color%}%{$fg[cyan]%}%c%{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%}% %{$reset_color%}: "
}

switch_aws() {
  local profiles=($(grep -w '\[.*\]' ~/.aws/credentials | tr -d '[]'))

  for i in {1..$#profiles}; do
    echo "${i}. ${profiles[i]}"
  done

  echo "Select AWS profile you want to switch: "
  read selected_answer

  local selected_value=${profiles[$selected_answer]}

  export AWS_PROFILE=$selected_value
  reset_prompt
}

switch() {
  local switch_type=$1

  if [ "$1"="aws" ]; then
    switch_aws
  else
    exit 0
  fi
}

reset_prompt