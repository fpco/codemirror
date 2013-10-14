// Workaround for missing functionality in IE 8 and earlier.
if( Object.create === undefined ) {
	Object.create = function( o ) {
	    function F(){}
	    F.prototype = o;
	    return new F();
	};
}

CodeMirror.defineMode("haskell", function(config) {

  function switchState(source, state, setState, f) {
    setState(f, state);
    return f(source, state, setState);
  }

  // These should all be Unicode extended, as per the Haskell 2010 report
  var smallRE = /[a-z_]/;
  var largeRE = /[A-Z]/;
  var digitRE = /[0-9]/;
  var hexitRE = /[0-9A-Fa-f]/;
  var octitRE = /[0-7]/;
  var qualRE = /[a-z_A-Z0-9'\.]*\./;
  var idRE = /[a-z_A-Z0-9']/;
  var symbolRE = /[-!#$%&*+.\/<=>?@\\^|~:]/;
  var specialRE = /[(),;[\]`{}]/;
  var whiteCharRE = /[ \t\v\f]/; // newlines are handled in tokenizer
  var layoutKeyword = /let|where|of|do/;


  function block(column, indentation) {
    return function(source, state, setState) {
      var column = source.column() + CodeMirror.countColumn(source.current(), null, source.tabSize);
      // If we're at the beginning of the next line after a block opener, then use normal mode.
      if (column == 0) {
        return switchState(source, state, setState, normal);
      // Otherwise, the position after eating whitespace determines the indent.
      // (f = do     bar), the start of bar is the start of the block.
      //TODO: handle comments etc.
      } else {
        source.eatWhile(whiteCharRE)
        if (!source.eol()) {
          // Remove the old indent.
          state.scopes.shift();
          // NOTE: need to recompute column because it might have changed due to eating whitespace.
          column = source.column() + CodeMirror.countColumn(source.current(), null, source.tabSize);
          setState(normal, state, { offset: column, isBlock: true });
        }
        return null;
      }
    };
  }

  // Returns null if the indent shouldn't be recorded.
  //
  // FIXME: For some reason selections in the middle of indents cause the nodes
  // to get split..
  function indentTag(column, isBlock, isEnd, isSuggestion) {
    if (!config.showAlignments || isSuggestion) { return false; }
    return "indent indent-" + column +
      (column % config.indentUnit == 0 ? " ontab-indent" : " offtab-indent") +
      (isBlock ? " block-indent" : " plain-indent") +
      (isEnd ? " endent" : "");
  }

  function normal(source, state, setState) {
    var column = source.column() + CodeMirror.countColumn(source.current(), null, source.tabSize);
    // Store this line's indentation if we're at the beginning.
    if (column == 0) {
      var indentation = source.indentation();
      state.indentation = indentation;
      // Remove scopes that no longer matter.
      var topScope;
      while (topScope = state.scopes.shift()) {
        if (topScope.startBlock) {
          // Found the start of a block, need to put it back on the scopes stack.
          state.scopes.unshift({ offset: indentation, isBlock: true });
          break;
        } else if (indentation >= topScope.offset) {
          // This indent should be left alone - it's at a lower / equal indent.
          state.scopes.unshift(topScope);
          break;
        }
      }
    }
    var matched = whiteCharRE.exec(source.peek());
    if (matched) {
      if (column < state.indentation) {
        for (var i = state.scopes.length - 1; i >= 0; i--) {
          // Eat whitespace till it reaches the next indentation.
          var offset = state.scopes[i].offset;
          var isBlock = state.scopes[i].isBlock;
          var isSuggestion = state.scopes[i].isSuggestion;
          while (column < offset) {
            var eaten = source.eat(whiteCharRE);
            if (!eaten) break;
            column += CodeMirror.countColumn(eaten, null, source.tabSize);
          }
          // Output an "indent" span, allowing indent guides to be styled.
          if (source.current().length > 0) {
            if (column == offset && column != 0 && whiteCharRE.test(source.peek())) {
              var result = indentTag(column, isBlock, false, isSuggestion);
              if (result) return result;
            // If we're at the end of indentation, but it's a block indent, then
            // mark that span as an indent.  We know we're at the end because if
            // the loop was able to eat enough whitespace, column == offset.
            } else if (column + 1 == offset && source.current().length > 0 && isBlock) {
              // Need to re-add the state.
              setState(normal, state, { offset: state.indentation });
              return indentTag(column, true, true, isSuggestion);
            }
          }
        }
        // Went past any indentation offset - record a plain indent.
        if (source.eatWhile(whiteCharRE) || source.current().length > 0) {
          setState(normal, state, { offset: state.indentation });
          column = source.column() + CodeMirror.countColumn(source.current(), null, source.tabSize);
          return indentTag(column, false, true, false)
        }
      }
      if (source.eatWhile(whiteCharRE)) {
        return null;
      }
    }

    //TODO: Don't indent after module header.
    if (source.match(layoutKeyword)) {
      if (source.eol() || whiteCharRE.test(source.peek)) {
        var indent = state.indentation + config.indentUnit;
        setState(block(column, indent), state, { offset: indent, startBlock: true });
        return "variable";
      } else {
        source.backUp(source.current().length);
      }
    }

    var ch = source.next();
    if (specialRE.test(ch)) {
      if (ch == '{' && source.eat('-')) {
        var t = "comment";
        if (source.eat('#')) {
          t = "meta";
        }
        return switchState(source, state, setState, ncomment(state, t, 1));
      }
      return null;
    }

    if (ch == '\'') {
      if (source.eat('\\')) {
        source.next();  // should handle other escapes here
      }
      else {
        source.next();
      }
      if (source.eat('\'')) {
        return "string";
      }
      return "error";
    }

    if (ch == '"') {
      return switchState(source, state, setState, stringLiteral);
    }

    if (largeRE.test(ch) && source.match(qualRE)) {
      return "qualifier";
    }
    if (largeRE.test(ch)) {
      source.eatWhile(idRE);
      return "variable-2";
    }

    if (smallRE.test(ch)) {
      source.eatWhile(idRE);
      return "variable";
    }

    if (digitRE.test(ch)) {
      if (ch == '0') {
        if (source.eat(/[xX]/)) {
          source.eatWhile(hexitRE); // should require at least 1
          return "integer";
        }
        if (source.eat(/[oO]/)) {
          source.eatWhile(octitRE); // should require at least 1
          return "number";
        }
      }
      source.eatWhile(digitRE);
      var t = "number";
      if (source.eat('.')) {
        t = "number";
        source.eatWhile(digitRE); // should require at least 1
      }
      if (source.eat(/[eE]/)) {
        t = "number";
        source.eat(/[-+]/);
        source.eatWhile(digitRE); // should require at least 1
      }
      return t;
    }

    if (symbolRE.test(ch)) {
      if (ch == '-' && source.eat(/-/)) {
        source.eatWhile(/-/);
        if (!source.eat(symbolRE)) {
          source.skipToEnd();
          return "comment";
        }
      }
      var t = "operator";
      if (ch == ':') {
        t = "operator-2";
      }
      source.eatWhile(symbolRE);
      // Auto-indent after symbols end the prior line.
      // TODO: special case out -> in types.
      // TODO: similar auto-indent for "if" blocks
      // TODO: handle trailing whitespace
      if (source.eol()) {
        setState(normal, state, { offset: state.indentation + config.indentUnit, isSuggestion: true });
      }
      return t;
    }

    return "error";
  }

  function ncomment(state, type, nest) {
    if (nest == 0) {
      return normal;
    }
    return function(source, state, setState) {
      var currNest = nest;
      while (!source.eol()) {
        var ch = source.next();
        if (ch == '{' && source.eat('-')) {
          ++currNest;
        }
        else if (ch == '-' && source.eat('}')) {
          --currNest;
          if (currNest == 0) {
            setState(normal, state);
            return type;
          }
        }
      }
      setState(ncomment(state, type, currNest), state);
      return type;
    };
  }

  function stringLiteral(source, state, setState) {
    while (!source.eol()) {
      var ch = source.next();
      if (ch == '"') {
        setState(normal, state);
        return "string";
      }
      if (ch == '\\') {
        if (source.eol() || source.eat(whiteCharRE)) {
          setState(stringGap, state);
          return "string";
        }
        if (source.eat('&')) {
        }
        else {
          source.next(); // should handle other escapes here
        }
      }
    }
    setState(normal, state);
    return "error";
  }

  function stringGap(source, state, setState) {
    if (source.eat('\\')) {
      return switchState(source, state, setState, stringLiteral);
    }
    source.next();
    setState(normal, state);
    return "error";
  }


  var wellKnownWords = (function() {
    var wkw = Object.create(null);
    function setType(t) {
      return function () {
        for (var i = 0; i < arguments.length; i++)
          wkw[arguments[i]] = t;
      };
    }

    setType("keyword")(
      "case", "class", "data", "default", "deriving", "do", "else", "foreign",
      "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
      "module", "newtype", "of", "then", "type", "where", "_");

    setType("keyword")(
      "\.\.", ":", "::", "=", "\\", "\"", "<-", "->", "@", "~", "=>");

    /*
    setType("builtin")(
      "!!", "$!", "$", "&&", "+", "++", "-", ".", "/", "/=", "<", "<=", "=<<",
      "==", ">", ">=", ">>", ">>=", "^", "^^", "||", "*", "**");

    setType("builtin")(
      "Bool", "Bounded", "Char", "Double", "EQ", "Either", "Enum", "Eq",
      "False", "FilePath", "Float", "Floating", "Fractional", "Functor", "GT",
      "IO", "IOError", "Int", "Integer", "Integral", "Just", "LT", "Left",
      "Maybe", "Monad", "Nothing", "Num", "Ord", "Ordering", "Rational", "Read",
      "ReadS", "Real", "RealFloat", "RealFrac", "Right", "Show", "ShowS",
      "String", "True");

    setType("builtin")(
      "abs", "acos", "acosh", "all", "and", "any", "appendFile", "asTypeOf",
      "asin", "asinh", "atan", "atan2", "atanh", "break", "catch", "ceiling",
      "compare", "concat", "concatMap", "const", "cos", "cosh", "curry",
      "cycle", "decodeFloat", "div", "divMod", "drop", "dropWhile", "either",
      "elem", "encodeFloat", "enumFrom", "enumFromThen", "enumFromThenTo",
      "enumFromTo", "error", "even", "exp", "exponent", "fail", "filter",
      "flip", "floatDigits", "floatRadix", "floatRange", "floor", "fmap",
      "foldl", "foldl1", "foldr", "foldr1", "fromEnum", "fromInteger",
      "fromIntegral", "fromRational", "fst", "gcd", "getChar", "getContents",
      "getLine", "head", "id", "init", "interact", "ioError", "isDenormalized",
      "isIEEE", "isInfinite", "isNaN", "isNegativeZero", "iterate", "last",
      "lcm", "length", "lex", "lines", "log", "logBase", "lookup", "map",
      "mapM", "mapM_", "max", "maxBound", "maximum", "maybe", "min", "minBound",
      "minimum", "mod", "negate", "not", "notElem", "null", "odd", "or",
      "otherwise", "pi", "pred", "print", "product", "properFraction",
      "putChar", "putStr", "putStrLn", "quot", "quotRem", "read", "readFile",
      "readIO", "readList", "readLn", "readParen", "reads", "readsPrec",
      "realToFrac", "recip", "rem", "repeat", "replicate", "return", "reverse",
      "round", "scaleFloat", "scanl", "scanl1", "scanr", "scanr1", "seq",
      "sequence", "sequence_", "show", "showChar", "showList", "showParen",
      "showString", "shows", "showsPrec", "significand", "signum", "sin",
      "sinh", "snd", "span", "splitAt", "sqrt", "subtract", "succ", "sum",
      "tail", "take", "takeWhile", "tan", "tanh", "toEnum", "toInteger",
      "toRational", "truncate", "uncurry", "undefined", "unlines", "until",
      "unwords", "unzip", "unzip3", "userError", "words", "writeFile", "zip",
      "zip3", "zipWith", "zipWith3");
    */

    return wkw;
  })();

  return {
    startState: function ()  { return { f: normal, scopes: [{offset: 0, type: "haskell"}] }; },
    copyState:  function (s) { return { f: s.f, scopes: s.scopes.concat([]) }; },

    token: function(stream, state) {
      //TODO: fix this callback madness.
      var t = state.f(stream, state,
        function(s, state, indent) {
          state.f = s;
          if (indent) {
            if (state.scopes.length == 0) {
              state.scopes.unshift(indent);
            } else {
              for (var i = 0; i < state.scopes.length; i++) {
                var topScope = state.scopes[i];
                if (topScope.offset <= indent.offset) {
                  var exists = topScope.offset == indent.offset;
                  if (!(exists && topScope.isBlock)) {
                    state.scopes.splice(i, exists ? 1 : 0, indent);
                  }
                  break;
                }
              }
            }
          }
        });
      var w = stream.current();
      return (w in wellKnownWords) ? wellKnownWords[w] : t;
    },

    //FIXME: If the user makes a newline right after a token where its
    // indentation is sensitive to being on the eol (e.g., operators), then it
    // won't be considered.
    indent: function(state) {
      for (var i = 0; i < state.scopes.length; i++) {
        return state.scopes[i].offset;
      }
      return CodeMirror.Pass;
    }
  };

});

CodeMirror.defineMIME("text/x-haskell", "haskell");
