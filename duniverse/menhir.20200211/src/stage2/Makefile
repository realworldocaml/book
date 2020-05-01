# The directory used by dune to mirror this directory.

BUILD_DIR := ../../_build/default/src/stage2

# [make update] is used under the programmer's manual control, after the
# grammar in [parser.mly] has been modified.

# It updates the file [parserMessages.messages] with new error states (if
# necessary) and with new auto-generated comments for all error states.

.PHONY: update
update:
	@ dune build --force parserMessages.messages.updated
	@ cp $(BUILD_DIR)/parserMessages.messages.updated parserMessages.messages

# [make strip] strips away the auto-generated comments found in the file
# parserMessages.messages. It is typically used after [make update], which
# creates many such comments.

.PHONY: strip
strip:
	@ sed -e "/^##/d" -i.bak parserMessages.messages
	@ rm parserMessages.messages.bak
