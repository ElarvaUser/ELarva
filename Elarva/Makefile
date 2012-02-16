EBIN_DIR=ebin
SOURCE_DIR=src
INCLUDE_DIR=include
ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -I $(INCLUDE_DIR) -o $(EBIN_DIR) $(ERLC_FLAGS) $(SOURCE_DIR)
ERL=erl -I -pa ebin -noshell -eval
LEXER_NAME=lexer
PARSER_NAME=parser

compile:
	mkdir -p $(EBIN_DIR)
	# Compile the lexer generator because it is not part of OTP
	$(ERLC)/leex.erl
	# Generate the lexer
	$(ERL) 'leex:file("$(SOURCE_DIR)/$(LEXER_NAME)",[{outdir,"$(SOURCE_DIR)"}]), halt().'
	# Generate the parser
	$(ERL) 'yecc:file("$(SOURCE_DIR)/$(PARSER_NAME)"), halt().'
	# Compile everything
	$(ERLC)/*.erl

all: compile

clean:
	rm $(SOURCE_DIR)/$(LEXER_NAME).erl
	rm $(SOURCE_DIR)/$(PARSER_NAME).erl
	rm $(EBIN_DIR)/*.beam
