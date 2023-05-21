# Hosting a local Minecraft server

## Minecraft port

Make sure that you have `server-port` set to the port `25565` in the
`server.properties` config file located at `/srv/papermc`

## Add listening port

First make sure you have `firewalld` installed, and `firewalld.service`
enabled/started. We shall open listening ports `25565` (which follows the TCP
protocol) for Java Edition connections and `19123` (which follows the UDP
protocol) for Bedrock Edition connections:
```
firewall-cmd --zone=public --add-port={25565/tcp,19123/udp}
```

## Configuring router port-forwarding

Take a look at the local IP of your router with:
```
ip address show
```
it will be located in the entry `wlan0` for wifi connections
(e.g. `"192.168.0.16/24"`). Now that you have the IP of your router, you can open
its address in your browser. Sign in to your router, go to advanced options and
select port forwarding. You should now configure the initial ports for both
external and internal connections as `25565` with TCP protocol and set the final
port again to `25565` (now you should also create the forwarding of the `19123` port
with UDP protocol in the same way as with `25565`).

## `firewalld` port-forwarding

Now that we have configured the router to forward these ports, we should make
sure that `firewalld` also know about this:
```
# 25565 port-forwarding for both ipv4 and ipv6
firewall-cmd --zone=public --add-rich-rule='rule family=ipv4 forward-port port=25565 protocol=tcp to-port=25565'

firewall-cmd --zone=public --add-rich-rule='rule family=ipv6 forward-port port=19123 protocol=tcp to-port=25565'

# 19123 port-forwarding for both ipv4 and ipv6
firewall-cmd --zone=public --add-rich-rule='rule family=ipv4 forward-port port=19123 protocol=udp to-port=19123'

firewall-cmd --zone=public --add-rich-rule='rule family=ipv6 forward-port port=19123 protocol=udp to-port=19123'
```

# Observations

It should be noted that this `firewalld` related configuration will undone as soon as the system
reboots, this is why I've created the script `open-mine-server`. If you wish to
make it permanent, just add the flag `--permanent` to the above commands.
