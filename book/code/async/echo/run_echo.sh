jbuilder build echo.exe
./_build/default/echo.exe &
sleep 1
echo "This is an echo server" | nc 127.0.0.1 8765
echo "It repeats whatever I write" | nc 127.0.0.1 8765
killall -9 echo.exe
