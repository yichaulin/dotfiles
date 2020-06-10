switch_aws() {
  local profiles=($(grep -w '\[.*\]' ~/.aws/credentials | tr -d '[]'))

  for i in {1..$#profiles}; do
    echo "${i}. ${profiles[i]}"
  done

  echo "------------------------------------"
  echo "Select AWS profile you want to switch: "
  read selected_answer

  local selected_value=${profiles[$selected_answer]}

  export AWS_PROFILE=$selected_value
}

switch_k8s() {
  local k8s_roles=(uat-operator prod-operator)

  for i in {1..$#k8s_roles}; do
    echo "${i}. ${k8s_roles[i]}"
  done

  echo "------------------------------------"
  echo "Select K8s role you want to switch: "
  read selected_answer

  local selected_value=${k8s_roles[$selected_answer]}
  kubectl config use-context $selected_value
  
  if [[ $selected_value == prod-* ]]; then
    export AWS_PROFILE="mdaq-prod"
  else
    export AWS_PROFILE="mdaq-nonprod"
  fi
}

switch() {
  local switch_type=$1

  if [ "$switch_type" = "aws" ]; then
    echo $switch_type
    switch_aws
  elif [ "$switch_type" = "k8s" ]; then
    switch_k8s
  else
    exit 0
  fi
}
