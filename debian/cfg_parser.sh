#!/bin/bash
cfg.parser ()
{
    PREFIX=${2:=INI_}
    ini="$(<$1)"                # read the file
    ini="${ini//\[/\\\[}"          # escape [
    ini="${ini//\]/\\\]}"          #escape ]
    IFS=$'\n' && ini=( ${ini} ) # convert to line-array
    ini=( ${ini[*]//;*/} )      # remove comments with ;
    ini=( ${ini[*]/\    =/=} )  # remove tabs before =
    ini=( ${ini[*]/=\   /=} )   # remove tabs be =
    ini=( ${ini[*]/\ =\ /=} )   # remove anything with a space around =
    ini=( ${ini[*]/#/$PREFIX} ) # add prefix to allow numeric names
    ini=( ${ini[*]/$PREFIX\\[/\\[} ) # add prefix to allow numeric names
    ini=( ${ini[*]/#\\[/\}$'\n'cfg.section.} ) # set section prefix
    ini=( ${ini[*]/%\\]/ \(} )    # convert text2function (1)
    ini=( ${ini[*]/=/=\( } )    # convert item to array
    ini=( ${ini[*]/%/ \)} )     # close array parenthesis
    ini=( ${ini[*]/%\\ \)/ \\} ) # the multiline trick
    ini=( ${ini[*]/%\( \)/\(\) \{} ) # convert text2function (2)
    ini=( ${ini[*]/%\} \)/\}} ) # remove extra parenthesis
    ini[0]="" # remove first element
    ini[${#ini[*]} + 1]='}'    # add the last brace
    eval "$(echo "${ini[*]}")" # eval the result
}
#cfg.parser './pydecades/decades.ini' DECADES_
#cfg.section.Servers
#seq $DECADES_slave_base_port $(($DECADES_slave_base_port+$DECADES_slaves-1))
