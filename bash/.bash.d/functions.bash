
add-path()
{
    var=$1
    del-path $var $@
    shift
    for path in $@
    do
        declare -gx $var="$path${!var:+":${!var}"}"
    done
}

del-path()
{
    var=$1
    shift
    for path in $@
    do
        declare -gx $var=${!var//":$path:"/:} #delete all instances in the middle
        declare -gx $var=${!var/%":$path"/} #delete any instance at the end
        declare -gx $var=${!var/#"$path:"/} #delete any instance at the beginning
        declare -gx $var=${!var//"$path"/} #delete singleton instance
    done
}
