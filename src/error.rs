#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    UnexpectedToken(usize, String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "Unexpected End of Input"),
            Self::UnexpectedToken(offset, msg) => {
                write!(f, "Unexpected token starting at byte {}: {}", offset, msg)
            }
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Clone, Copy)]
pub struct LineInfo {
    pub start: usize,
    pub length: usize,
    pub offset: usize,
    pub number: usize,
}

pub fn find_line(orig: &[u8], offset: usize) -> Option<LineInfo> {
    let pre = orig.get(..offset)?;
    let to_prev_nl = pre
        .iter()
        .rev()
        .enumerate()
        .find_map(find_nl)
        .unwrap_or(pre.len());
    let start = pre.len().saturating_sub(to_prev_nl);
    let post = orig.get(offset..)?;
    let to_next_nl = post
        .iter()
        .enumerate()
        .find_map(find_nl)
        .unwrap_or(post.len());
    let length = orig[start..offset + to_next_nl].len();
    let number = pre.split(is_nl).count();
    Some(LineInfo {
        start,
        length,
        offset: to_prev_nl,
        number,
    })
}
fn find_nl((i, b): (usize, &u8)) -> Option<usize> {
    if is_nl(b) {
        Some(i)
    } else {
        None
    }
}

fn is_nl(b: &u8) -> bool {
    *b == b'\n' || *b == b'\r' || *b == 0xFF
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn find_new_line() {
        let s = b"asdf\nasdf";
        let nl = s.iter().enumerate().find_map(find_nl).unwrap();
        assert_eq!(nl, 4);
    }

    #[test]
    fn find_lines() {
        let byte_offset_targets = &[
            ("This only has 1 line", 7, "This only has 1 line"),
            ("this has\na few lines\nin it", 12, "a few lines"),
        ];
        for &(bytes, offset, target) in byte_offset_targets {
            let info = find_line(bytes.as_bytes(), offset).unwrap();
            assert_eq!(&bytes[info.start..info.start + info.length], target);
        }
    }
}
