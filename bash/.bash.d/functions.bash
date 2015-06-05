
del-path()
{
    PATH=${PATH//":$1"/} # delete in the middle or at the end
    PATH=${PATH//"$1:"/} # delete at the beginning
}

add-path()
{
    del-path $1
    PATH="$1:$PATH" # prepend to beginning
}
