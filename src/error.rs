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
    let to_prev_nl = pre.iter().rev().enumerate().find_map(find_nl).unwrap_or(0);
    let start = pre.len().saturating_sub(to_prev_nl);
    let post = orig.get(offset..)?;
    let length = post
        .iter()
        .enumerate()
        .find_map(find_nl)
        .unwrap_or(post.len());
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
