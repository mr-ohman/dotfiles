 #!/usr/bin/env bash
 
 synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')
