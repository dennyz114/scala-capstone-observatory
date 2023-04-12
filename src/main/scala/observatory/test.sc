

val action = Seq("010013,,,")

action.map(_.split(','))
action.map(_.split(",", -1)) 
