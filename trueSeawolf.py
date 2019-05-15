#Jason McEntee #108069440
tokens = (
    'STRING','INTEGER','REAL',
    'PLUS','MINUS','TIMES','DIVIDE','EXPONENT','MODULO','FDIVIDE',
    'LPAREN','RPAREN','LBRACKET','RBRACKET','COMMA','SEMICOLON','LCBRACKET','RCBRACKET',
    'NOT','AND','OR',
    'GREATER','GEQUAL','EQUAL','EQUALS','NEQUAL','LEQUAL','LESS',
    'IN',
    'WHILE','IF','ELSE','PRINT','NAME',
    )
reserved = {
    'while' : 'WHILE',
    'if' : 'IF',
    'else' : 'ELSE',
    'print' : 'PRINT',
    'in' : 'IN',
    'and' : 'AND',
    'or' : 'OR',
    'not' : 'NOT'
}

# Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = r','
t_NOT = r'not'
t_AND = r'and'
t_OR = r'or'
t_EXPONENT = r'\*\*'
t_MODULO = r'%'
t_FDIVIDE = r'//'
t_LESS = r'<'
t_GREATER = r'>'
t_EQUALS = r'='
t_EQUAL = r'=='
t_NEQUAL = r'<>'
t_LEQUAL = r'<='
t_GEQUAL = r'>='
t_SEMICOLON = r';'
t_LCBRACKET = r'\{'
t_RCBRACKET = r'\}'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    if t.value in reserved:
      t.type = reserved[t.value]
    return t

def t_STRING(t):
    r'\"(\"\"|[^"])*\"'
    t.value = str(t.value[1:-1])
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_REAL(t):
    r'\d*\.\d*'
    t.value = float(t.value)
    return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

import ply.lex as lex
lexer = lex.lex()

# Parsing rules

precedence = (
    ('right','COMMA'),
    ('left','OR'),
    ('left','AND'),
    ('right','NOT'),
    ('left','LESS','LEQUAL','EQUAL','NEQUAL','GEQUAL','GREATER'),
    ('left','IN'),
    ('left','PLUS','MINUS'),
    ('left','FDIVIDE'),
    ('left','EXPONENT'),
    ('left','MODULO'),
    ('left','TIMES','DIVIDE'),
    )

names = { }

def p_statement_assign_var(t):
    'statement : NAME rvalue'
    t[0] = ('assign-var',t[1],t[2])   

def p_rvalue(t):
    'rvalue : EQUALS expression SEMICOLON'
    t[0] = ('rvalue',t[2])

def p_statement_assign_index(t):
    'statement : NAME location_tail'
    t[0] = ('assign-index',t[1],t[2])

def p_location_index_tail(t):
    'location_tail : LBRACKET expression RBRACKET location_tail'
    t[0] = ('location-index-tail',t[2],t[4])

def p_location_index_end(t):
    'location_tail : LBRACKET expression RBRACKET rvalue'
    t[0] = ('location-index-end',t[2],t[4])

def p_statement_print(t):
    'statement : PRINT LPAREN expression RPAREN SEMICOLON'
    t[0] = ('print-statement',t[3])

def p_expression_index(t):
    'expression : expression LBRACKET expression RBRACKET'
    t[0] = ('index-expression',t[1],t[3])

def p_expression_list_tail(t):
    'expression : expression COMMA expression'
    t[0] = ('list-tail',t[1],t[3])

def p_expression_list(t):
    'expression : LBRACKET expression RBRACKET'
    t[0] = ('list',t[2])

def p_block_tail_block_end(t):
    'block_tail : statement RCBRACKET'
    t[0] = ('block-end',t[1])

def p_block_tail_block_tail(t):
    'block_tail : statement block_tail'
    t[0] = ('block-tail',t[1],t[2])

def p_statement_block_start(t):
    'statement : LCBRACKET block_tail'
    t[0] = ('block-start',t[2])

def p_statement_while(t):
    'statement : WHILE LPAREN expression RPAREN statement'
    t[0] = ('while-statement',t[3],t[5])

def p_statement_if(t):
    'statement : IF LPAREN expression RPAREN statement'
    t[0] = ('if-statement',t[3],t[5])

def p_statement_if_else(t):
    'statement : IF LPAREN expression RPAREN statement ELSE statement'
    t[0] = ('if-else-statement',t[3],t[5],t[7])

def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression AND expression
                  | expression OR expression
                  | expression MODULO expression
                  | expression EXPONENT expression
                  | expression FDIVIDE expression
                  | expression GREATER expression
                  | expression GEQUAL expression
                  | expression EQUAL expression
                  | expression NEQUAL expression
                  | expression LEQUAL expression
                  | expression LESS expression
                  | expression IN expression'''
    t[0] = ('binary-expression',t[2],t[1],t[3])

def p_expression_unop(t):
    'expression : NOT expression'
    t[0] = ('unary-expression',t[1],t[2])

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = ('group-expression',t[2])

def p_expression_number(t):
    '''expression : INTEGER
                  | REAL'''
    t[0] = ('number',t[1])

def p_expression_string(t):
    'expression : STRING'
    t[0] = ('string',t[1])

def p_expression_name(t):
    'expression : NAME'
    t[0] = ('name',t[1])

def p_statement_print_error(t):
     'expression : error'
     t[0] = "SYNTAX ERROR"

def p_error(t):
    pass

import sys
import ply.yacc as yacc
parser = yacc.yacc()

def areNumbers(a,b):
  return a == 'number' and b == 'number'

def traverseAST(node):
  rule = node[0]
  if rule == 'if-statement':
    if(traverseAST(node[1])[1]):
      traverseAST(node[2])
    return
  if rule == 'if-else-statement':
    if(traverseAST(node[1])[1]):
      traverseAST(node[2])
    else:
      traverseAST(node[3])
    return
  if rule == 'while-statement':
    while(traverseAST(node[1])[1]):
      traverseAST(node[2])
    return
  if rule == 'print-statement':
    arg1 = traverseAST(node[1])
    if arg1[0] == 'list':
      print(arg1[2],end="")
    else:
      print(arg1[1],end="")
    return
  if rule == 'rvalue':
    return traverseAST(node[1])
  if rule == 'assign-var':
    names[node[1]] = traverseAST(node[2])
    return
  if rule == 'assign-index':
    if node[1] in names:
      arg1 = names[node[1]]
      if arg1[0] == 'list':
        return traverseAST(node[2]+(arg1,))
  if rule == 'location-index-tail':
    print(node[3])
    arg1 = traverseAST(node[1])
    if arg1[0] == 'number':
      arg2 = (node[3][1])[arg1[1]]
      if arg2[0] == 'list':
        return traverseAST(node[2]+(arg2,))
  if rule == 'location-index-end': 
    arg1 = traverseAST(node[1])
    if arg1[0] == 'number':
      (node[3][1])[arg1[1]] = traverseAST(node[2])
      (node[3][2])[arg1[1]] = traverseAST(node[2])[1]
      return
  if rule == 'name':
    if node[1] in names:
      return names[node[1]]
  if rule == 'string' or rule == 'number':
    return (rule,node[1])
  if rule == 'group-expression':
    return traverseAST(node[1])
  if rule == 'unary-expression':
    arg1 = traverseAST(node[2])
    if node[1] == 'not' and arg1[0] == 'number':
      return ('number',int(not arg1[1]))
  if rule == 'binary-expression':
    arg1 = traverseAST(node[2])
    arg2 = traverseAST(node[3])
    if node[1] == '+' and arg1[0] == arg2[0]:
      if arg1[0] == 'list':
        return (arg1[0],arg1[1] + arg2[1],arg1[2] + arg2[2])
      return (arg1[0],arg1[1] + arg2[1])
    if node[1] == '-' and areNumbers(arg1[0],arg2[0]):
      return ('number',arg1[1] - arg2[1])
    if node[1] == '*' and areNumbers(arg1[0],arg2[0]):
      return ('number',arg1[1] * arg2[1])
    if node[1] == '/' and areNumbers(arg1[0],arg2[0]) and arg2 != 0:
      return ('number',arg1[1] / arg2[1])
    if node[1] == '%' and areNumbers(arg1[0],arg2[0]):
      return ('number',arg1[1] % arg2[1])
    if node[1] == '**' and areNumbers(arg1[0],arg2[0]):
      return ('number',arg1[1] - arg2[1])
    if node[1] == '//' and areNumbers(arg1[0],arg2[0]):
      return ('number',arg1[1] - arg2[1])
    if node[1] == '<' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] < arg2[1]))
    if node[1] == '<=' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] <= arg2[1]))
    if node[1] == '==' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] == arg2[1]))
    if node[1] == '<>' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] != arg2[1]))
    if node[1] == '>' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] > arg2[1]))
    if node[1] == '>=' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] >= arg2[1]))
    if node[1] == 'or' and areNumbers(arg1[0],arg2[0]):
      return ('number',int(arg1[1] or arg2[1]))
    if node[1] == 'and' and areNumbers(arg1[0],arg2[0]):
      if (arg1[1] and arg2[1]) == 0:
        return ('number',0)
      return ('number',1)
    if node[1] == 'in':
      if arg2[0] == 'list':
        return ('number',int(arg1[1] in arg2[2]))
      if arg2[0] == 'string':
        return ('number',int(arg1[1] in arg2[1]))
  if rule == 'list':
    arg1 = traverseAST(node[1])
    if arg1[0]!='list-tail':
      arg1 = (arg1[0],[arg1],[arg1[1]]) 
    return ('list',arg1[1],arg1[2])
  if rule == 'list-tail':
    arg1 = traverseAST(node[1])
    arg2 = traverseAST(node[2])
    if arg1[0]!='list-tail':
      arg1 = (arg1[0],[arg1],[arg1[1]])
    if arg2[0]!='list-tail':
      arg2 = (arg2[0],[arg2],[arg2[1]])
    return ('list-tail',arg1[1] + arg2[1],arg1[2] + arg2[2])
  if rule == 'index-expression':
    arg1 = traverseAST(node[1])
    arg2 = traverseAST(node[2])
    if arg1[0] == 'list' and arg2[0] == 'number':
      tup = arg1[1][arg2[1]]
      return (tup[0],tup[1])
    elif arg1[0] == 'string' and arg2[0] == 'number':
      return ('string',arg1[1][arg2[1]])
  if rule == 'block-start':
    traverseAST(node[1])
    return
  if rule == 'block-tail':
    traverseAST(node[1])
    traverseAST(node[2])
    return
  if rule == 'block-end':
    traverseAST(node[1])
    return
  if node == "SYNTAX ERROR":
    print("SYNTAX ERROR")
    sys.exit(0)
  print("SEMANTIC ERROR")
  sys.exit(0)

f = open(sys.argv[1],"r")
s = f.read()
result = parser.parse(s)
traverseAST(result)
if (result == "SEMANTIC ERROR" or result == "SYNTAX ERROR"):
 print(result)
f.close()