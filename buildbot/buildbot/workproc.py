import curio
import os


async def handle(client, addr):
    print('connected')
    while True:
        async for line in client.makefile('rb'):
            print(line)
    print('closed')


def workproc(sockpath='workproc.sock'):
    try:
        curio.run(curio.unix_server, sockpath, handle)
    except KeyboardInterrupt:
        pass
    finally:
        os.unlink(sockpath)


if __name__ == '__main__':
    workproc()
