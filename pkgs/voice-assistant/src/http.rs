//! HTTP GET via curl: system CAs, proxies and IPv6 for free, without
//! pulling a TLS stack into the binary.

use crate::skills::output;

const USER_AGENT: &str = concat!("voice-assistant/", env!("CARGO_PKG_VERSION"));

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ip {
    Any,
    V4,
    V6,
}

#[derive(Debug, Clone)]
pub struct Request<'a> {
    url: &'a str,
    headers: Vec<&'a str>,
    timeout_secs: u32,
    ip: Ip,
}

/// A GET with the shared defaults: follow redirects, fail on HTTP errors, 6 s.
pub fn get(url: &str) -> Request<'_> {
    Request {
        url,
        headers: Vec::new(),
        timeout_secs: 6,
        ip: Ip::Any,
    }
}

impl<'a> Request<'a> {
    pub fn header(mut self, h: &'a str) -> Self {
        self.headers.push(h);
        self
    }

    pub fn timeout(mut self, secs: u32) -> Self {
        self.timeout_secs = secs;
        self
    }

    pub fn ip(mut self, ip: Ip) -> Self {
        self.ip = ip;
        self
    }

    fn args(&self) -> Vec<String> {
        let mut a: Vec<String> = ["-sfL", "--max-time"].map(str::to_owned).into();
        a.push(self.timeout_secs.to_string());
        a.extend(["-A".to_owned(), USER_AGENT.to_owned()]);
        match self.ip {
            Ip::Any => {}
            Ip::V4 => a.push("-4".into()),
            Ip::V6 => a.push("-6".into()),
        }
        for h in &self.headers {
            a.extend(["-H".to_owned(), (*h).to_owned()]);
        }
        // Keep a URL starting with "-" from being read as an option.
        a.extend(["--".to_owned(), self.url.to_owned()]);
        a
    }

    /// The trimmed body, or `None` on network/HTTP errors or an empty reply.
    pub fn text(&self) -> Option<String> {
        let args = self.args();
        let refs: Vec<&str> = args.iter().map(String::as_str).collect();
        output("curl", &refs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_curl_args() {
        let a = get("https://x.test/")
            .header("Accept: text/plain")
            .timeout(3)
            .ip(Ip::V6)
            .args();
        assert_eq!(
            a,
            [
                "-sfL",
                "--max-time",
                "3",
                "-A",
                USER_AGENT,
                "-6",
                "-H",
                "Accept: text/plain",
                "--",
                "https://x.test/"
            ]
        );
        assert_eq!(get("u").args()[2], "6");
    }
}
