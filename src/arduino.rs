use crate::{
    basic::{interpreter::BasicCommand, lexer::String},
    BasicProgram,
};
use arduino_hal::{
    hal::port::PB3,
    port::{
        mode::{self, Floating},
        Pin,
    },
    prelude::*,
    simple_pwm::{IntoPwmPin, Timer2Pwm},
    Eeprom,
};
use avr_progmem::progmem_display as D;
use ufmt::{uwrite, uwriteln};

pub type Serial = arduino_hal::hal::usart::Usart0<arduino_hal::DefaultClock>;

pub fn init() -> (Pins, Serial, Eeprom) {
    let dp = arduino_hal::Peripherals::take().unwrap();

    let adc = arduino_hal::Adc::new(dp.ADC, Default::default());
    let mut adc_opt = Some(adc);

    let timer = Timer2Pwm::new(dp.TC2, arduino_hal::simple_pwm::Prescaler::Prescale64);

    let eeprom = arduino_hal::Eeprom::new(dp.EEPROM);

    let pins = arduino_hal::pins!(dp);

    let analog = [
        pins.a0
            .into_analog_input(adc_opt.as_mut().unwrap())
            .into_channel(),
        pins.a1
            .into_analog_input(adc_opt.as_mut().unwrap())
            .into_channel(),
        pins.a2
            .into_analog_input(adc_opt.as_mut().unwrap())
            .into_channel(),
        pins.a3
            .into_analog_input(adc_opt.as_mut().unwrap())
            .into_channel(),
        pins.a4
            .into_analog_input(adc_opt.as_mut().unwrap())
            .into_channel(),
        pins.a5
            .into_analog_input(adc_opt.as_mut().unwrap())
            .into_channel(),
    ];

    let adc = adc_opt.take().unwrap();

    (
        Pins {
            adc,
            analog,
            digital_input: [
                Some(pins.d2.into_floating_input().downgrade()),
                Some(pins.d3.into_floating_input().downgrade()),
                Some(pins.d4.into_floating_input().downgrade()),
                Some(pins.d5.into_floating_input().downgrade()),
                Some(pins.d6.into_floating_input().downgrade()),
                Some(pins.d7.into_floating_input().downgrade()),
                None,
                None,
                None,
                None,
                None,
                None,
            ],
            digital_output: [
                None,
                None,
                None,
                None,
                None,
                None,
                Some(pins.d8.into_output().downgrade()),
                Some(pins.d9.into_output().downgrade()),
                Some(pins.d10.into_output().downgrade()),
                None,
                Some(pins.d12.into_output().downgrade()),
                Some(pins.d13.into_output().downgrade()),
            ],
            pwm: pins.d11.into_output().into_pwm(&timer),
        },
        arduino_hal::default_serial!(dp, pins, 57600),
        eeprom,
    )
}

pub struct Pins {
    adc: arduino_hal::Adc,
    analog: [arduino_hal::adc::Channel; 6],
    digital_input: [Option<Pin<mode::Input<Floating>>>; 12],
    digital_output: [Option<Pin<mode::Output>>; 12],
    pwm: Pin<mode::PwmOutput<Timer2Pwm>, PB3>,
}

pub enum PinError {
    NonAddressable,
    NotInput,
    NotOutput,
    NotPwm,
}

impl Pins {
    fn check_pwm(&self, pin_num: u8) -> Result<(), PinError> {
        if pin_num == 11 {
            Ok(())
        } else {
            Err(PinError::NotPwm)
        }
    }
    fn check_digital(&self, pin_num: u8) -> Result<(), PinError> {
        match (2..=13).contains(&pin_num) {
            false => Err(PinError::NonAddressable),
            true if pin_num == 11 => Err(PinError::NonAddressable),
            true => Ok(()),
        }
    }
    fn check_analog(&self, pin_num: u8) -> Result<(), PinError> {
        match (0..=5).contains(&pin_num) {
            true => Ok(()),
            false => Err(PinError::NonAddressable),
        }
    }

    pub fn make_digital_output(&mut self, pin_num: u8) -> Result<(), PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num as usize - 2;
        match self.digital_input[pin_num].take() {
            Some(pin) => {
                _ = self.digital_output[pin_num].insert(pin.into_output().downgrade());
                Ok(())
            }
            None => Err(PinError::NotInput),
        }
    }
    pub fn make_digital_input(&mut self, pin_num: u8) -> Result<(), PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num as usize - 2;
        match self.digital_output[pin_num].take() {
            Some(pin) => {
                _ = self.digital_input[pin_num].insert(pin.into_floating_input().downgrade());
                Ok(())
            }
            None => Err(PinError::NotOutput),
        }
    }

    pub fn write_analog(&mut self, pin_num: u8, duty: u8) -> Result<(), PinError> {
        self.check_pwm(pin_num)?;

        if duty == 0 {
            self.pwm.disable();
        } else {
            self.pwm.set_duty(duty);
            self.pwm.enable();
        }
        Ok(())
    }
    pub fn read_analog(&mut self, pin_num: u8) -> Result<u16, PinError> {
        self.check_analog(pin_num)?;

        Ok(self.adc.read_blocking(&self.analog[pin_num as usize]))
    }
    pub fn read_digital(&mut self, pin_num: u8) -> Result<bool, PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num as usize - 2;
        if let Some(ref mut pin) = self.digital_input[pin_num] {
            Ok(pin.is_high())
        } else {
            Err(PinError::NotInput)
        }
    }
    pub fn write_digital(&mut self, pin_num: u8, value: bool) -> Result<(), PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num as usize - 2;
        if let Some(ref mut pin) = self.digital_output[pin_num] {
            match value {
                true => pin.set_high(),
                false => pin.set_low(),
            }
            Ok(())
        } else {
            Err(PinError::NotOutput)
        }
    }
}

pub enum EepromError {
    SaveProgram,
    SaveString,
    LoadProgram,
    LoadString,
}

pub fn eeprom_save(
    serial: &mut Serial,
    eeprom: &mut Eeprom,
    program: &BasicProgram,
    string_table: &[Option<String>; 8],
) -> Result<(), EepromError> {
    let mut offset = 0u16;

    // SAFETY
    // lily (my blahaj) has looked at this code VERY hard and she says its ok :3
    // guarenteed by lily herself :3c

    uwrite!(serial, "{}", D!("program ")).unwrap_infallible();
    let mut command_buf: [u8; size_of::<Option<BasicCommand>>()];
    for command in program {
        command_buf = unsafe {
            core::mem::transmute_copy::<Option<BasicCommand>, [u8; size_of::<Option<BasicCommand>>()]>(
                command,
            )
        };
        for j in command_buf.as_slice().windows(1) {
            let existing = eeprom.read_byte(offset);
            if existing != j[0] {
                eeprom
                    .write(offset, j)
                    .map_err(|_| EepromError::SaveProgram)?;
                uwrite!(serial, "{}", D!(":")).unwrap_infallible();
            } else {
                uwrite!(serial, "{}", D!(".")).unwrap_infallible();
            }
            offset += 1;
        }
    }
    uwriteln!(serial, "{}", D!("\r")).unwrap_infallible();

    uwrite!(serial, "{}", D!("string table ")).unwrap_infallible();
    let mut string_buf: [u8; size_of::<Option<String>>()];
    for string in string_table {
        string_buf = unsafe {
            core::mem::transmute_copy::<Option<String>, [u8; size_of::<Option<String>>()]>(string)
        };
        for j in string_buf.as_slice().windows(1) {
            let existing = eeprom.read_byte(offset);
            if existing != j[0] {
                eeprom
                    .write(offset, j)
                    .map_err(|_| EepromError::SaveString)?;
                uwrite!(serial, "{}", D!(":")).unwrap_infallible();
            } else {
                uwrite!(serial, "{}", D!(".")).unwrap_infallible();
            }
            offset += 1;
        }
    }
    uwriteln!(serial, "{}", D!("\r\ndone.\r\n\r")).unwrap_infallible();

    Ok(())
}

pub fn eeprom_load(
    serial: &mut Serial,
    eeprom: &Eeprom,
    program: &mut BasicProgram,
    string_table: &mut [Option<String>; 8],
) -> Result<(), EepromError> {
    let mut offset = 0u16;

    // SAFETY
    // lily has also blessed this function to work every time :3
    // (i love her sm :333)
    uwrite!(serial, "{}", D!("program ")).unwrap_infallible();
    let mut command_buf = [0u8; size_of::<Option<BasicCommand>>()];
    for command in program {
        eeprom
            .read(offset, &mut command_buf)
            .map_err(|_| EepromError::LoadProgram)?;
        *command = unsafe {
            core::mem::transmute_copy::<[u8; size_of::<Option<BasicCommand>>()], Option<BasicCommand>>(
                &command_buf,
            )
        };
        for _ in 0..size_of::<Option<BasicCommand>>() {
            uwrite!(serial, "{}", D!(".")).unwrap_infallible();
            offset += 1;
        }
    }
    uwriteln!(serial, "{}", D!("\r")).unwrap_infallible();

    uwrite!(serial, "{}", D!("string table ")).unwrap_infallible();
    let mut string_buf = [0u8; size_of::<Option<String>>()];
    for string in string_table {
        eeprom
            .read(offset, &mut string_buf)
            .map_err(|_| EepromError::LoadString)?;
        *string = unsafe {
            core::mem::transmute_copy::<[u8; size_of::<Option<String>>()], Option<String>>(
                &string_buf,
            )
        };
        for _ in 0..size_of::<Option<String>>() {
            uwrite!(serial, "{}", D!(".")).unwrap_infallible();
            offset += 1;
        }
    }
    uwriteln!(serial, "{}", D!("\r\ndone.\r\n\r")).unwrap_infallible();

    Ok(())
}
