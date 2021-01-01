
# Prattle

TCP server with client implementation made for fun to learn Erlang. Server is an OTP application, whereas client is pure erlang module.

- [Prattle](#prattle)
  * [Requirements](#requirements)
  * [Build](#build)
  * [Running the app](#running-the-app)
    + [Starting erlang shell with app modules](#starting-erlang-shell-with-app-modules)
    + [Running client or server](#running-client-or-server)
  * [Using the client](#using-the-client)
  * [App architecture](#app-architecture)

## Requirements
If you want to build the app and run it:
- Erlang/OTP (project was built using version 23)
- Rebar3

If you download compiled beam files from release section, only erlang is needed.


## Build
    $ make all

## Running the app

### Starting erlang shell with app modules
You will need to run erlang shell first. If you run make command from build section, you can start it this way:

    $ make shell

If you downloaded the release files, start erlang shell inside the directory with beam files

    $ erl -pa ./path_to_dir_with_beam

### Running client or server
Both client and server will need their separate shells, to start client:

    > prattle_client:start(). 

To run server:

    > prattle_app:start().

Client and server run on your local host, on port 8000, make sure it is free. If you need to use another one, use the following commands instead.

    > prattle_client:start({{127,0,0,1}, 9000}) # Replace IP address and port as needed

    > prattle_app:start({9000}) # Server always listens on localhost, you can only customize the port. 


## Using the client

First, client will connect to lobby, until you join a room three commands are available.

- join:room_name - it will join existing room or create one for you and join it
- room:list - it will send you a message with existing rooms
- prattle:leave - app will exit

Connection must be estabilished, otherwise client will try to recconect, and commands will not be accepted. You can only send break signal to terminal to exit the erlang shell (cmd/ctrl + C)

After you join the chat room, you have another 2 commands to use:
- name:YOUR_NAME - changes your name inside the chat, otherwise generated proccess PID is used
- leave:room - will get you back to lobby

Any other input will be treated as a message, and it will be sent to all connected clients.

## App architecture
Simplified visualization, see longer explanation below.
![App architecture](static/architecture.png?raw=true)

1. Main app supervisor spawns server proccess and room supervisor.
2. Client app connects to server, and is handled by spawned lobby connection, a separate proccess.
3. After room join request is sent, server asks room supervisor for the list of room proccesses, and suitable room will be found or created. Then, server will send back room port which was found.
4. Client will connect to the sent port. Room proccess will spawn client connection which will handle message exchange with the client. 
5. If message is sent from the client, connection proccess will send it to the room proccess, and it will send broadcast message to all of its active connections, which will in turn send it back to the connected clients, and the message will be shown. 
5. If the client requests room leave, client connection will be destroyed, and client will connect to server once again. If the client was the last one, and the room is empty, it will terminate.

All of this actions will be logged to the server console. Feel free to explore the code and its comments for more explanations.
