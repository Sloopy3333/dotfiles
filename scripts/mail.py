#!/bin/python3.9
# import required libraries
import imaplib
import email

username = ""
password = ""
imap = imaplib.IMAP4_SSL("imap.gmail.com")

result = imap.login(username, password)
imap.select('inbox', readonly = True)
response, messages = imap.search(None, 'UnSeen')
messages = messages[0].split()
latest = int(messages[-1])
oldest = int(messages[0])

# fetch
res, msg = imap.fetch(str(latest), "(RFC822)")

for response in msg:
    if isinstance(response, tuple):
        msg = email.message_from_bytes(response[1])
        # print required information
        print(msg["Date"])
        print(msg["From"])
        print(msg["Subject"])

for part in msg.walk():
    if part.get_content_type() == "text / plain":
        # get text or plain data
        body = part.get_payload(decode=True)
        print(f'Body: {body.decode("UTF-8")}', )
print(len(messages))

for i in range(5):
    print(i)
