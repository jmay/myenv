# http://brettterpstra.com/a-simple-but-handy-bash-function-console/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+BrettTerpstra+%28Brett+Terpstra%29
function console () {
  if [[ $# > 0 ]]; then
    query=$(echo "$*"|tr -s ' ' '|')
    tail -100f /var/log/system.log|grep -i --color=auto -E "$query"
  else
    tail -100f /var/log/system.log
  fi
}

source ~/env/na/na.sh
