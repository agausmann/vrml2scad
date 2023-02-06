#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    Comma,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Number(&'a str),
    Ident(&'a str),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn tokenize<'a>(s: &'a str) -> Vec<Token<'a>> {
    let mut remainder = s.trim();

    let mut tokens = Vec::new();

    while let Some(head) = remainder.chars().next() {
        let (len, token) = match head {
            ',' => (1, Token::Comma),
            '[' => (1, Token::OpenBracket),
            ']' => (1, Token::CloseBracket),
            '{' => (1, Token::OpenBrace),
            '}' => (1, Token::CloseBrace),
            '-' | '+' | '.' | '0'..='9' => {
                let mut pos = 0;

                // Parse optional leading sign
                if remainder[pos..].starts_with(['-', '+']) {
                    pos += 1;
                }

                // Parse digits before decimal
                pos += remainder[pos..]
                    .char_indices()
                    .skip_while(|(_, c)| c.is_ascii_digit())
                    .next()
                    .map(|(i, _)| i)
                    .unwrap_or(remainder[pos..].len());

                // Parse optional decimal and additional digits
                if remainder[pos..].starts_with('.') {
                    pos += 1;

                    pos += remainder[pos..]
                        .char_indices()
                        .skip_while(|(_, c)| c.is_ascii_digit())
                        .next()
                        .map(|(i, _)| i)
                        .unwrap_or(remainder[pos..].len());
                }

                // Parse optional exponent, sign and digits
                if remainder[pos..].starts_with(['e', 'E']) {
                    pos += 1;

                    if remainder[pos..].starts_with(['-', '+']) {
                        pos += 1;
                    }

                    pos += remainder[pos..]
                        .char_indices()
                        .skip_while(|(_, c)| c.is_ascii_digit())
                        .next()
                        .map(|(i, _)| i)
                        .unwrap_or(remainder[pos..].len());
                }

                let number = &remainder[..pos];
                // TODO graceful error
                assert!(
                    number.chars().any(|c| c.is_ascii_digit()),
                    "numeric symbols with no digits: {:?}",
                    number
                );
                (pos, Token::Number(number))
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let len = remainder
                    .char_indices()
                    .skip_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
                    .next()
                    .map(|(i, _)| i)
                    .unwrap_or(remainder.len());

                (len, Token::Ident(&remainder[..len]))
            }
            '#' => {
                // Skip comment
                let len = remainder
                    .char_indices()
                    .find(|(_, c)| "\r\n".contains(*c))
                    .map(|(i, _)| i)
                    .unwrap_or(remainder.len());
                remainder = &remainder[len..].trim_start();
                continue;
            }
            other => {
                // TODO graceful error
                panic!("unexpected input {:?}", other);
            }
        };

        tokens.push(token);
        remainder = &remainder[len..].trim_start();
    }

    tokens
}
