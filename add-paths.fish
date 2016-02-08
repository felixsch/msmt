
set work "./.stack-work/dist/x86_64-linux/Cabal-1.22.6.0/build"
set -gx PATH $PATH $work/msmt $work/msmt-cli $work/msmt-common $work/msmt-frontend $work/msmt-proxy

function fish_right_prompt
     echo -n -s (set_color -b blue white) "(MSMT)" (set_color normal) " "
end
