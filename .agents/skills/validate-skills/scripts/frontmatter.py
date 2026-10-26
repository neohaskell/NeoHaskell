import json
import re


NONSTRING_PLAIN_SCALAR = re.compile(
    r'^(?:'
    r'null|~|true|false|yes|no|on|off|'
    r'[-+]?(?:0|[1-9][0-9_]*|0[0-7_]*|0[xX][0-9a-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+)|'
    r'[-+]?(?:[0-9][0-9_]*\.[0-9_]*|\.[0-9][0-9_]*|[0-9][0-9_]*[eE][-+]?[0-9]+|'
    r'[0-9][0-9_]*\.[0-9_]*[eE][-+]?[0-9]+|'
    r'[0-9][0-9_]*(?::[0-5]?[0-9])+(?:\.[0-9_]*)?)|'
    r'[-+]?\.(?:inf|nan)|'
    r'[0-9]{4}-[0-9]{2}-[0-9]{2}(?:[Tt ][0-9]{2}:[0-9]{2}(?::[0-9]{2}(?:\.[0-9]+)?)?(?:[Zz]|[+-][0-9]{2}(?::?[0-9]{2})?)?)?'
    r')$',
    re.IGNORECASE,
)


def parse_frontmatter_scalar(raw, line_no):
    value = raw.strip()
    if not value:
        return ('', None)
    if value.startswith('"'):
        try:
            parsed = json.loads(value)
        except json.JSONDecodeError as exc:
            return (None, f'frontmatter line {line_no} has invalid quoted YAML scalar: {exc.msg}')
        if not isinstance(parsed, str):
            return (None, f'frontmatter line {line_no} must be a string scalar')
        return (parsed, None)
    if value.startswith("'"):
        if len(value) < 2 or not value.endswith("'"):
            return (None, f'frontmatter line {line_no} has an unterminated quoted scalar')
        inner = value[1:-1]
        parsed = []
        index = 0
        while index < len(inner):
            if inner[index] != "'":
                parsed.append(inner[index])
                index += 1
            elif index + 1 < len(inner) and inner[index + 1] == "'":
                parsed.append("'")
                index += 2
            else:
                return (None, f'frontmatter line {line_no} has an unescaped apostrophe in a single-quoted scalar')
        return (''.join(parsed), None)
    plain_indicators = '-?:,[]{}#&*!|>\'"%@`'
    if value[0] in plain_indicators or re.search(':\\s|\\s#', value):
        return (None, f'frontmatter line {line_no} is not in the safe plain-scalar subset; quote YAML-significant text')
    if NONSTRING_PLAIN_SCALAR.fullmatch(value):
        return (None, f'frontmatter line {line_no} must be a string scalar; quote YAML non-string values')
    return (value, None)


def parse_frontmatter(text):
    lines = text.splitlines()
    if not lines or lines[0] != '---':
        return (None, ['missing opening `---` frontmatter delimiter'])
    closing = next((index for (index, line) in enumerate(lines[1:], start=1) if line == '---'), None)
    if closing is None:
        return (None, ['missing closing `---` frontmatter delimiter'])
    fields = {}
    errors = []
    index = 1
    while index < closing:
        line = lines[index]
        line_no = index + 1
        if not line.strip() or line.lstrip().startswith('#') or line[:1].isspace():
            index += 1
            continue
        match = re.fullmatch('([A-Za-z0-9_-]+):\\s*(.*)', line)
        if not match:
            errors.append(f'frontmatter line {line_no} is not a top-level YAML mapping entry')
            index += 1
            continue
        (key, raw_value) = match.groups()
        if key not in {'name', 'description'}:
            index += 1
            continue
        if key in fields:
            errors.append(f'frontmatter key `{key}` is duplicated')
            index += 1
            continue
        marker = raw_value.strip()
        if re.fullmatch('[|>](?:[1-9][+-]?|[+-][1-9]?)?', marker):
            block = []
            index += 1
            while index < closing and (not lines[index].strip() or lines[index][:1].isspace()):
                block.append(lines[index].strip())
                index += 1
            value = ('\n' if marker.startswith('|') else ' ').join(block).strip()
            fields[key] = value
            continue
        (value, scalar_error) = parse_frontmatter_scalar(raw_value, line_no)
        if scalar_error:
            errors.append(scalar_error)
        else:
            fields[key] = value
        index += 1
    return (fields, errors)
