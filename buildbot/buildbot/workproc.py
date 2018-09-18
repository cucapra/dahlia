import curio
import os


class WorkProc:
    async def handle(self, client, addr):
        """Handle an incoming socket connection.
        """
        while True:
            async for line in client.makefile('rb'):
                print(line)

    def serve(self, sockpath='workproc.sock'):
        """Start listening on a Unix domain socket for incoming
        messages. Run indefinitely (until the server is interrupted).
        """
        try:
            curio.run(curio.unix_server, sockpath, self.handle)
        except KeyboardInterrupt:
            pass
        finally:
            os.unlink(sockpath)


if __name__ == '__main__':
    WorkProc().serve()
