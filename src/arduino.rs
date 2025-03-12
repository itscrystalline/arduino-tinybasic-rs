use arduino_hal::{
    eeprom::OutOfBoundsError,
    port::{
        mode::{self, Floating},
        Pin,
    },
    Eeprom,
};
use serde::Serialize;

use crate::{basic::interpreter::BasicCommand, BasicProgram, PROGRAM_LENGTH};

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

pub fn eeprom_save(eeprom: &mut Eeprom, program: &BasicProgram) -> Result<usize, EepromError> {
    let config = bincode::config::standard();
    let mut buf = [0u8; 1024];
    let saved = bincode::encode_into_slice(program, &mut buf, config)
        .map_err(|_| EepromError::SaveEncode)?;

    eeprom.write(0, &buf).map_err(|_| EepromError::SaveStore)?;
    Ok(saved)
}

pub fn eeprom_load(eeprom: &Eeprom, program: &mut BasicProgram) -> Result<usize, EepromError> {
    let config = bincode::config::standard();
    let mut buf = [0u8; 1024];
    eeprom.read(0, &mut buf).map_err(|_| EepromError::Load)?;

    let (read, size) = bincode::decode_from_slice::<BasicProgram, _>(&buf, config)
        .map_err(|_| EepromError::LoadDecode)?;
    *program = read;

    Ok(size)
}
