IRB.conf[:PROMPT][:MY_PROMPT] = { # name of prompt mode
  :AUTO_INDENT => true,            # enables auto-indent mode
  :PROMPT_I => IRB.conf[:PROMPT][:DEFAULT][:PROMPT_I], # keep normal prompt
  :PROMPT_S => "",               # prompt for continuated strings
  :PROMPT_C => "",               # prompt for continuated statement
  :RETURN => "=> %s\n"        # format to return value
}

IRB.conf[:PROMPT_MODE] = :MY_PROMPT
