# These are the states that jobs can be in. The transitions are:
#
#
#       UPLOAD
#         |
#       UNPACK---------------+
#         |                  |
#    UNPACK_FINISH           |
#         |                MAKE
#       COMPILE              |
#         |                  |
#    COMPILE_FINISH          |
#         |                MAKE_PROGRESS----+
#        HLS                 |              |
#         |                  |          STARTING_AFI (F1 ONLY)
#         |                  |              |
#      HLS_FINISH -----------+----------GENERATING_AFI (F1 ONLY)             
#         |
#        RUN
#         |
#   +-----+-----+
#   |           |
# DONE         FAIL


UPLOAD = "uploaded"
UNPACK = "unpacking"
UNPACK_FINISH = "unpacked"
COMPILE = "seashelling"
COMPILE_FINISH = "seashelled"
HLS = "hlsing"
HLS_FINISH = "hlsed"
MAKE = "make"
MAKE_PROGRESS = "makeing"
RUN = "fpga_executing"
DONE = "done"
FAIL = "failed"
AFI_START = "starting_AFI"
AFI = "generating_AFI"