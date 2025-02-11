#![no_std]
#![no_main]

use panic_halt as _;

#[arduino_hal::entry]
fn main() -> ! {
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);

    let mut led_red = pins.d11.into_output();
    let mut led_green = pins.d10.into_output();
    let mut led_orange = pins.d8.into_output();

    loop {
        led_red.set_high();
        //arduino_hal::delay_ms(1000);
        led_green.set_high();
        led_orange.set_high();
        arduino_hal::delay_ms(1000);
        led_red.set_low();
        //arduino_hal::delay_ms(1000);
        led_green.set_low();
        led_orange.set_low();
        arduino_hal::delay_ms(1000);
    }
}
