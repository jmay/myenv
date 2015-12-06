# http://brettterpstra.com/a-simple-but-handy-bash-function-console/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+BrettTerpstra+%28Brett+Terpstra%29
function console () {
  if [[ $# > 0 ]]; then
    query=$(echo "$*"|tr -s ' ' '|')
    tail -100f /var/log/system.log|grep -i --color=auto -E "$query"
  else
    tail -100f /var/log/system.log
  fi
}

function status_sigil {
  if [[ $1 -eq 0 ]]
  then
    sigil=☺
    sigilcolor=$color_green
  else
    sigil=☹
    sigilcolor=$color_red
  fi
}

source ~/env/na/na.sh


# from http://brettterpstra.com/2013/02/10/the-joy-of-sshfs/
# Remote Mount (sshfs)
# creates mount folder and mounts the remote filesystem
rmount() {
  local host folder mname
  host="${1%%:*}:"
  [[ ${1%:} == ${host%%:*} ]] && folder='' || folder=${1##*:}
  if [[ $2 ]]; then
    mname=$2
  else
    mname=${folder##*/}
    [[ "$mname" == "" ]] && mname=${host%%:*}
  fi
  if [[ $(grep -i "host ${host%%:*}" ~/.ssh/config) != '' ]]; then
    mkdir -p ~/mounts/$mname > /dev/null
    sshfs $host$folder ~/mounts/$mname -oauto_cache,reconnect,defer_permissions,negative_vncache,volname=$mname,noappledouble && echo "mounted ~/mounts/$mname"
  else
    echo "No entry found for ${host%%:*}"
    return 1
  fi
}

# Remote Umount, unmounts and deletes local folder (experimental, watch you step)
rumount() {
  if [[ $1 == "-a" ]]; then
    ls -1 ~/mounts/|while read dir
    do
      [[ $(mount|grep "mounts/$dir") ]] && umount ~/mounts/$dir
      [[ $(ls ~/mounts/$dir) ]] || rm -rf ~/mounts/$dir
    done
  else
    [[ $(mount|grep "mounts/$1") ]] && umount ~/mounts/$1
    [[ $(ls ~/mounts/$1) ]] || rm -rf ~/mounts/$1
  fi
}

# from http://unix.stackexchange.com/questions/11856/sort-but-keep-header-line-in-the-at-the-top
body() {
    IFS= read -r header
    printf '%s\n' "$header"
    "$@"
}
