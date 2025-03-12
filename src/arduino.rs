use crate::{
    basic::{interpreter::BasicCommand, lexer::String},
    BasicProgram, PROGRAM_LENGTH,
};
use arduino_hal::{
    port::{
        mode::{self, Floating},
        Pin,
    },
    prelude::*,
    Eeprom,
};
use avr_progmem::progmem_display as D;
use ufmt::{uwrite, uwriteln};

pub type Serial = arduino_hal::hal::usart::Usart0<arduino_hal::DefaultClock>;

pub fn init() -> (Pins, Serial, Eeprom) {
    let dp = arduino_hal::Peripherals::take().unwrap();
    Pins::init(dp)
}

pub struct Pins {
    adc: arduino_hal::Adc,
    analog: [arduino_hal::adc::Channel; 6],
    digital_input: [Option<Pin<mode::Input<Floating>>>; 12],
    digital_output: [Option<Pin<mode::Output>>; 12],
    //pwm: [Option<Pin<mode::PwmOutput<TC>>>; 12],
}

pub enum PinError {
    Reserved,
    NonAddressable,
    NotInput,
    NotOutput,
    NotPwm,
}

impl Pins {
    pub fn init(dp: arduino_hal::Peripherals) -> (Self, Serial, Eeprom) {
        let adc = arduino_hal::Adc::new(dp.ADC, Default::default());
        let eeprom = arduino_hal::Eeprom::new(dp.EEPROM);
        let mut adc_opt = Some(adc);
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
            Self {
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
                    Some(pins.d11.into_output().downgrade()),
                    Some(pins.d12.into_output().downgrade()),
                    Some(pins.d13.into_output().downgrade()),
                ],
                //pwm: [const { None }; 12],
            },
            arduino_hal::Usart::new(
                dp.USART0,
                pins.d0,
                pins.d1.into_output(),
                arduino_hal::hal::usart::BaudrateArduinoExt::into_baudrate(57600),
            ),
            eeprom,
        )
    }

    fn check_digital(&self, pin_num: usize) -> Result<(), PinError> {
        match (2..=13).contains(&pin_num) {
            true => Ok(()),
            false => Err(PinError::NonAddressable),
        }
    }
    fn check_analog(&self, pin_num: usize) -> Result<(), PinError> {
        match (0..=5).contains(&pin_num) {
            true => Ok(()),
            false => Err(PinError::NonAddressable),
        }
    }

    pub fn make_digital_output(&mut self, pin_num: usize) -> Result<(), PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num - 2;
        match self.digital_input[pin_num].take() {
            Some(pin) => {
                _ = self.digital_output[pin_num].insert(pin.into_output().downgrade());
                Ok(())
            }
            None => Err(PinError::NotInput),
        }
    }
    pub fn make_digital_input(&mut self, pin_num: usize) -> Result<(), PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num - 2;
        match self.digital_output[pin_num].take() {
            Some(pin) => {
                _ = self.digital_input[pin_num].insert(pin.into_floating_input().downgrade());
                Ok(())
            }
            None => Err(PinError::NotOutput),
        }
    }

    pub fn read_analog(&mut self, pin_num: usize) -> Result<usize, PinError> {
        self.check_analog(pin_num)?;

        Ok(self.adc.read_blocking(&self.analog[pin_num]) as usize)
    }
    pub fn read_digital(&mut self, pin_num: usize) -> Result<bool, PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num - 2;
        if let Some(ref mut pin) = self.digital_input[pin_num] {
            Ok(pin.is_high())
        } else {
            Err(PinError::NotInput)
        }
    }
    pub fn write_digital(&mut self, pin_num: usize, value: bool) -> Result<(), PinError> {
        self.check_digital(pin_num)?;

        let pin_num = pin_num - 2;
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
    SaveEncode,
    SaveStore,
    Load,
    LoadDecode,
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
    let mut command_buf: [u8; size_of::<Option<BasicCommand>>()];
    for command in program {
        command_buf = unsafe {
            core::mem::transmute_copy::<Option<BasicCommand>, [u8; size_of::<Option<BasicCommand>>()]>(
                command,
            )
        };
        command_buf.into_iter().for_each(|j| {
            eeprom.write_byte(offset, j);
            uwrite!(serial, "{}", D!(".")).unwrap_infallible();
            offset += 1;
        });
    }
    uwrite!(serial, "{}", D!(" | ")).unwrap_infallible();

    let mut string_buf: [u8; size_of::<Option<String>>()];
    for string in string_table {
        string_buf = unsafe {
            core::mem::transmute_copy::<Option<String>, [u8; size_of::<Option<String>>()]>(string)
        };
        string_buf.into_iter().for_each(|j| {
            eeprom.write_byte(offset, j);
            uwrite!(serial, "{}", D!(".")).unwrap_infallible();
            offset += 1;
        });
    }
    uwriteln!(serial, "{}", D!("done.\r\n\r")).unwrap_infallible();

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
    let mut command_buf = [0u8; size_of::<Option<BasicCommand>>()];
    for command in program {
        eeprom
            .read(offset, &mut command_buf)
            .map_err(|_| EepromError::Load)?;
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
    uwrite!(serial, "{}", D!(" | ")).unwrap_infallible();

    let mut string_buf = [0u8; size_of::<Option<String>>()];
    for string in string_table {
        eeprom
            .read(offset, &mut string_buf)
            .map_err(|_| EepromError::Load)?;
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
    uwriteln!(serial, "{}", D!("done.\r\n\r")).unwrap_infallible();

    Ok(())
}
