MIKAN_OUTPUT_DIR=output
MIKAN_OUTPUT_S_SRCS=$(wildcard $(MIKAN_OUTPUT_DIR)/*.S)
MIKAN_OUTPUT_S_OBJS=$(patsubst %.S,%.o,$(MIKAN_OUTPUT_S_SRCS))
MIKAN_RUNTIME_DIR=$(shell pwd)/runtime

init:
	@echo $(MIKAN_OUTPUT_S_OBJS)

.PHONY:support
support:
	@$(MAKE) -C $(MIKAN_RUNTIME_DIR) all

$(MIKAN_OUTPUT_S_OBJS): %.o: %.S
	@$(CC) -o $@ $< -L$(MIKAN_RUNTIME_DIR) -lmikan

.PHONY:all
all:support $(MIKAN_OUTPUT_S_OBJS)

.PHONY:clean
clean:
	@$(MAKE) -C $(MIKAN_RUNTIME_DIR) clean
	@$(RM) -f $(MIKAN_OUTPUT_S_OBJS)
