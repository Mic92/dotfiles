use std::cmp::{max, min};
use std::path::PathBuf;
use std::fs::{read_dir, read_to_string, write};
use std::thread::sleep;
use std::time::Duration;

const MINTEMP : i32 = 50000;
const MAXTEMP : i32 = 80000;
const MINSTART : i32 = 60;
const MINSTOP : i32 = 29;
const MAXPWM : i32 = 255;


fn adjust(fan: &PathBuf, sensor: &PathBuf) {
    let temp: i32 = read_to_string(sensor).unwrap().trim().parse().unwrap();
    let prev_pwm: i32 = read_to_string(fan).unwrap().trim().parse().unwrap();

    let mut pwm: i32;
    pwm = (temp - MINTEMP) * 255 / (MAXTEMP - MINTEMP);
    pwm = max(pwm, 0);
    if pwm > 0 {
        pwm = max(pwm, if prev_pwm < MINSTOP { MINSTART } else { MINSTOP });
        pwm = min(pwm, MAXPWM);
    }
    //println!("sensor: {}, pwm: {}", temp, pwm);
    write(fan, pwm.to_string()).unwrap();
}

fn main() {
    let mut fans = Vec::new();
    for hwmon_dir in ["/sys/devices/platform/fan1/hwmon", "/sys/devices/platform/fan2/hwmon"].iter() {
        for dir in read_dir(hwmon_dir).unwrap() {
            let mut p = dir.unwrap().path();
            p.push("pwm1");
            if p.exists() {
                fans.push(p)
            }
        }
    }
    let fans = fans;
    loop {
        for fan in &fans {
            for dir in read_dir("/sys/devices/virtual/thermal/thermal_zone0/").unwrap() {
                let mut p = dir.unwrap().path();
                if p.to_str().unwrap().contains("hwmon") {
                    p.push("temp1_input");
                    adjust(fan, &p)
                }
            }
        }
        sleep(Duration::from_secs(5));
    }
}
