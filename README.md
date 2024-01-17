# Simple chatting simulator

## How to run

Assuming *Haskell Stack* is installed, run the following commands:

```bash
git clone https://github.com/yoyololicon/qmul-ecs713-fp-final.git
cd qmul-ecs713-fp-final/
stack build

# Run the simulation with ten users and 100 messages
stack exec fp-final-exe 10 100
```

After the last command, you should see something like this:

```
Progress: 100/100
Summary:
User Frank received 8 messages (5 unread) and sent 8 messages (4 has been read).
User Wendy received 15 messages (8 unread) and sent 14 messages (6 has been read).
User Oscar received 6 messages (5 unread) and sent 13 messages (5 has been read).
User Mallory received 10 messages (7 unread) and sent 9 messages (5 has been read).
User Walter received 8 messages (6 unread) and sent 7 messages (5 has been read).
User Judy received 9 messages (3 unread) and sent 11 messages (4 has been read).
User Dave received 12 messages (8 unread) and sent 8 messages (5 has been read).
User Trent received 14 messages (2 unread) and sent 12 messages (3 has been read).
User Peggy received 9 messages (9 unread) and sent 8 messages (4 has been read).
User Bob received 9 messages (4 unread) and sent 10 messages (2 has been read).
```
