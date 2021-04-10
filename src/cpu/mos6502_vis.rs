use super::mos6502::{Asm, Cpu, RegSet, Vectors};

pub mod view {
    use super::*;
    use iced::{
        button, executor, keyboard, pane_grid, scrollable, Align, Application, Button, Color,
        Column, Command, Container, Element, HorizontalAlignment, Length, PaneGrid, Row,
        Scrollable, Subscription, Text,
    };
    use iced_native::{event, subscription, Event};

    use crate::bus::dummy_bus::*;

    use super::{Asm, Cpu, RegSet, Vectors};

    #[derive(Clone)]
    pub enum ViewType {
        CpuStateView(Option<RegSet>),
        MemView(u16, Vec<u8>),
        DisasmView(Option<Asm>),
        Empty,
    }

    impl std::fmt::Debug for ViewType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use ViewType::*;
            let which = match *&self {
                CpuStateView(_r) => "CPU State",
                MemView(_i, _v) => "Memory",
                DisasmView(_d) => "Disassembly",
                Empty => "MOS6502 Emulator - Nessy",
            };

            f.write_str(&which)
        }
    }

    impl std::fmt::Display for ViewType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use ViewType::*;
            match *&self {
                CpuStateView(state) => {
                    if let Some(regs) = state {
                        regs.fmt(f);
                    }
                }
                MemView(i, v) => {
                    let mut disp_addr: String;
                    let mut addr: u16 = *i;
                    for r in 0..16 {
                        disp_addr = format!("{:#06x}: ", addr);
                        for c in 0..16 {
                            let byte = v[r * 16 + c];
                            disp_addr.push_str(format!(" {:#04x}", byte).as_str());
                        }
                        f.write_str(&format!("{}\n", disp_addr))?;
                        addr += 16;
                    }
                }
                DisasmView(d) => {
                    if let Some(dis) = d {
                        f.write_str("\n\n");
                        for (l, code) in dis.map().iter() {
                            f.write_str(format!("{}\n", code).as_str())?;
                        }
                    }
                }
                _ => {}
            };
            Ok(())
        }
    }

    pub struct NessyGui {
        panes: pane_grid::State<Content>,
        panes_created: usize,
        focus: Option<pane_grid::Pane>,
        cpu: Option<Cpu>,
        view_kind: ViewType,
        pc: u16,
    }

    #[derive(Debug, Clone)]
    pub enum Message {
        Split(pane_grid::Axis, pane_grid::Pane, ViewType),
        SplitFocused(pane_grid::Axis, ViewType),
        FocusAdjacent(pane_grid::Direction),
        Clicked(pane_grid::Pane),
        Dragged(pane_grid::DragEvent),
        Resized(pane_grid::ResizeEvent),
        Close(pane_grid::Pane),
        CloseFocused,
        AdvancePC,
    }

    impl NessyGui {
        fn init_cpu() -> Option<Cpu> {
            let mut mos6502 = Cpu::new();
            let load_addr: u16 = 0x8000;
            let pc_initial = Vectors::RESET as u16;
            mos6502.writ_word(&pc_initial, &load_addr);

            mos6502.reset();

            let bus = mos6502.busline.as_mut().unwrap();
            let prog: &[u8] = &vec![
                0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
                0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA,
                0xEA, 0xEA,
            ];

            bus.load_prog(&load_addr, prog);

            Some(mos6502)
        }

        fn current_asm_range(&self) -> (u16, u16) {
            let pc = self.cpu.as_ref().unwrap().state.pc;
            let d: u16 = 0xffff - pc;
            if d > 32 {
                (pc, pc + 32)
            } else {
                (pc, pc + d)
            }
        }
    }

    impl Application for NessyGui {
        type Message = Message;
        type Executor = executor::Default;
        type Flags = ();

        fn new(_flags: ()) -> (Self, Command<Message>) {
            let (panes, _) = pane_grid::State::new(Content::new(0, ViewType::Empty));
            (
                NessyGui {
                    panes,
                    panes_created: 1,
                    focus: None,
                    cpu: NessyGui::init_cpu(),
                    view_kind: ViewType::Empty,
                    pc: 0x8000,
                },
                Command::none(),
            )
        }

        fn title(&self) -> String {
            String::from("MOS6502 emulator - Nessy")
        }

        fn update(&mut self, message: Message) -> Command<Message> {
            use ViewType::*;
            match message {
                Message::Split(axis, pane, vt) => {
                    let vt_res = match vt {
                        CpuStateView(_i) => CpuStateView(Some(self.cpu.as_ref().unwrap().state)),
                        DisasmView(_j) => {
                            let (first, second) = self.current_asm_range();
                            DisasmView(Some(
                                self.cpu.as_ref().unwrap().disasemble_region(first, second),
                            ))
                        }
                        MemView(k, v) => MemView(
                            0x8000,
                            self.cpu.as_ref().unwrap().read_region(0x8000, 0x80ff),
                        ),
                        Empty => Empty,
                    };

                    let result =
                        self.panes
                            .split(axis, &pane, Content::new(self.panes_created, vt_res));

                    if let Some((pane, _)) = result {
                        self.focus = Some(pane);
                    }

                    self.panes_created += 1;
                }
                Message::SplitFocused(axis, vt) => {
                    let vt_res = match vt {
                        CpuStateView(i) => CpuStateView(Some(self.cpu.as_ref().unwrap().state)),
                        DisasmView(j) => {
                            let (first, second) = self.current_asm_range();
                            DisasmView(Some(
                                self.cpu.as_ref().unwrap().disasemble_region(first, second),
                            ))
                        }
                        MemView(k, v) => MemView(
                            0x8000,
                            self.cpu.as_ref().unwrap().read_region(0x8000, 0x80ff),
                        ),
                        Empty => Empty,
                    };

                    if let Some(pane) = self.focus {
                        let result =
                            self.panes
                                .split(axis, &pane, Content::new(self.panes_created, vt_res));
                        if let Some((pane, _)) = result {
                            self.focus = Some(pane);
                        }

                        self.panes_created += 1;
                    }
                }
                Message::FocusAdjacent(direction) => {
                    if let Some(pane) = self.focus {
                        if let Some(adjacent) = self.panes.adjacent(&pane, direction) {
                            self.focus = Some(adjacent);
                        }
                    }
                }
                Message::Clicked(pane) => {
                    self.focus = Some(pane);
                }
                Message::Resized(pane_grid::ResizeEvent { split, ratio }) => {
                    self.panes.resize(&split, ratio);
                }
                Message::Dragged(pane_grid::DragEvent::Dropped { pane, target }) => {
                    self.panes.swap(&pane, &target);
                }
                Message::Dragged(_) => {}

                Message::AdvancePC => {
                    self.cpu.as_mut().unwrap().tick();
                }

                Message::Close(pane) => {
                    if let Some((_, sibling)) = self.panes.close(&pane) {
                        self.focus = Some(sibling);
                    }
                }
                Message::CloseFocused => {
                    if let Some(pane) = self.focus {
                        if let Some((_, sibling)) = self.panes.close(&pane) {
                            self.focus = Some(sibling);
                        }
                    }
                }
            }

            Command::none()
        }

        fn subscription(&self) -> Subscription<Message> {
            subscription::events_with(|event, status| {
                if let event::Status::Captured = status {
                    return None;
                }

                match event {
                    Event::Keyboard(keyboard::Event::KeyPressed {
                        modifiers,
                        key_code,
                    }) if modifiers.is_command_pressed() => handle_hotkey(key_code),
                    _ => None,
                }
            })
        }

        fn view(&mut self) -> Element<Message> {
            let focus = self.focus;
            let total_panes = self.panes.len();

            let pane_grid = PaneGrid::new(&mut self.panes, |pane, content| {
                let is_focused = focus == Some(pane);

                let title_bar = pane_grid::TitleBar::new(format!("{:?}", content.view_kind))
                    .padding(10)
                    .style(style::TitleBar { is_focused });

                pane_grid::Content::new(content.view(pane, total_panes))
                    .title_bar(title_bar)
                    .style(style::Pane { is_focused })
            })
            .width(Length::Fill)
            .height(Length::Fill)
            .spacing(10)
            .on_click(Message::Clicked)
            .on_drag(Message::Dragged)
            .on_resize(10, Message::Resized);

            Container::new(pane_grid)
                .width(Length::Fill)
                .height(Length::Fill)
                .padding(10)
                .into()
        }
    }

    fn handle_hotkey(key_code: keyboard::KeyCode) -> Option<Message> {
        use keyboard::KeyCode;
        use pane_grid::{Axis, Direction};

        let direction = match key_code {
            KeyCode::Up => Some(Direction::Up),
            KeyCode::Down => Some(Direction::Down),
            KeyCode::Left => Some(Direction::Left),
            KeyCode::Right => Some(Direction::Right),
            _ => None,
        };

        match key_code {
            KeyCode::W => Some(Message::CloseFocused),
            KeyCode::Space => Some(Message::AdvancePC),
            _ => direction.map(Message::FocusAdjacent),
        }
    }

    struct Content {
        id: usize,
        scroll: scrollable::State,
        view_kind: ViewType,
        close: button::State,
        disasm: button::State,
        mem: button::State,
        cpu_state: button::State,
        adv: button::State,
    }

    impl Content {
        fn new(id: usize, v: ViewType) -> Self {
            Content {
                id,
                view_kind: v,
                scroll: scrollable::State::new(),
                close: button::State::new(),
                disasm: button::State::new(),
                mem: button::State::new(),
                cpu_state: button::State::new(),
                adv: button::State::new(),
            }
        }
        fn view(&mut self, pane: pane_grid::Pane, total_panes: usize) -> Element<Message> {
            let Content {
                scroll,
                close,
                disasm,
                mem,
                cpu_state,
                adv,
                ..
            } = self;

            let button = |state, label, message, style| {
                Button::new(
                    state,
                    Text::new(label)
                        .width(Length::Fill)
                        .horizontal_alignment(HorizontalAlignment::Center)
                        .size(16),
                )
                .width(Length::Fill)
                .padding(8)
                .on_press(message)
                .style(style)
            };

            use pane_grid::Axis;

            let controls = Row::new()
                .spacing(5)
                .max_width(250)
                .push(button(
                    cpu_state,
                    "CPU",
                    Message::SplitFocused(
                        if total_panes % 2 != 0 {
                            Axis::Vertical
                        } else {
                            Axis::Horizontal
                        },
                        ViewType::CpuStateView(None),
                    ),
                    style::Button::Primary,
                ))
                .push(button(
                    mem,
                    "Mem",
                    Message::SplitFocused(
                        if total_panes % 2 != 0 {
                            Axis::Vertical
                        } else {
                            Axis::Horizontal
                        },
                        ViewType::MemView(100, Vec::new()),
                    ),
                    style::Button::Primary,
                ))
                .push(button(
                    disasm,
                    "ASM",
                    Message::SplitFocused(
                        if total_panes % 2 != 0 {
                            Axis::Vertical
                        } else {
                            Axis::Horizontal
                        },
                        ViewType::DisasmView(None),
                    ),
                    style::Button::Primary,
                ))
                .push(button(
                    close,
                    "X",
                    Message::Close(pane),
                    style::Button::Destructive,
                ))
                .push(button(
                    adv,
                    "Go",
                    Message::AdvancePC,
                    style::Button::Primary,
                ));

            let content = Scrollable::new(scroll)
                .width(Length::Fill)
                .spacing(10)
                .align_items(Align::Center)
                .push(controls)
                .push(
                    Column::new()
                        .padding(10)
                        .spacing(20)
                        .align_items(Align::Center)
                        .push(
                            Text::new(format!("{}", self.view_kind))
                                .size(18)
                                .color(Color::WHITE),
                        ),
                );

            Container::new(content)
                .width(Length::Fill)
                .height(Length::Fill)
                .padding(8)
                .into()
        }
    }

    mod style {
        use iced::{button, container, Background, Color, Vector};

        const SURFACE: Color = Color::from_rgb(
            0xF2 as f32 / 255.0,
            0xF3 as f32 / 255.0,
            0xF5 as f32 / 255.0,
        );

        const ACTIVE: Color = Color::from_rgb(
            0x72 as f32 / 255.0,
            0x89 as f32 / 255.0,
            0xDA as f32 / 255.0,
        );

        const HOVERED: Color = Color::from_rgb(
            0x67 as f32 / 255.0,
            0x7B as f32 / 255.0,
            0xC4 as f32 / 255.0,
        );

        const DARK_GREY: Color = Color::from_rgb(
            0x2c as f32 / 255.0,
            0x30 as f32 / 255.0,
            0x2e as f32 / 255.0,
        );

        const LIGHT_GREY: Color = Color::from_rgb(
            0x45 as f32 / 255.0,
            0x47 as f32 / 255.0,
            0x46 as f32 / 255.0,
        );

        const ULTRA_LIGHT_GREY: Color = Color::from_rgb(
            0xaa as f32 / 255.0,
            0xaf as f32 / 255.0,
            0xad as f32 / 255.0,
        );

        const SWAMP_GREEN: Color = Color::from_rgb(
            0x67 as f32 / 255.0,
            0x96 as f32 / 255.0,
            0x81 as f32 / 255.0,
        );

        pub struct TitleBar {
            pub is_focused: bool,
        }

        impl container::StyleSheet for TitleBar {
            fn style(&self) -> container::Style {
                let _pane = Pane {
                    is_focused: self.is_focused,
                }
                .style();

                container::Style {
                    text_color: Some(Color::WHITE),
                    background: Some(Background::Color(DARK_GREY)),
                    ..Default::default()
                }
            }
        }

        pub struct Pane {
            pub is_focused: bool,
        }

        impl container::StyleSheet for Pane {
            fn style(&self) -> container::Style {
                container::Style {
                    background: Some(Background::Color(LIGHT_GREY)),
                    border_width: 2.0,
                    border_color: if self.is_focused {
                        Color::BLACK
                    } else {
                        Color::from_rgb(0.7, 0.7, 0.7)
                    },
                    ..Default::default()
                }
            }
        }

        pub enum Button {
            Primary,
            Destructive,
        }

        impl button::StyleSheet for Button {
            fn active(&self) -> button::Style {
                let (background, text_color) = match self {
                    Button::Primary => (Some(LIGHT_GREY), Color::WHITE),
                    Button::Destructive => (None, Color::from_rgb8(0xFF, 0x47, 0x47)),
                };

                button::Style {
                    text_color,
                    background: background.map(Background::Color),
                    border_radius: 5.0,
                    shadow_offset: Vector::new(0.0, 0.0),
                    ..button::Style::default()
                }
            }

            fn hovered(&self) -> button::Style {
                let active = self.active();

                let background = match self {
                    Button::Primary => Some(SWAMP_GREEN),
                    Button::Destructive => Some(Color {
                        a: 0.2,
                        ..active.text_color
                    }),
                };

                button::Style {
                    background: background.map(Background::Color),
                    ..active
                }
            }
        }
    }
}
