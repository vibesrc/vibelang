//! Networking syscalls: socket, bind, listen, accept, connect, send, recv, etc.

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    // Type coercion helpers

    fn coerce_to_i32(&self, val: BasicValueEnum<'ctx>, context: &str) -> Result<inkwell::values::IntValue<'ctx>, CodegenError> {
        if val.is_int_value() {
            let int_val = val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                Ok(self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "coerce_i32"
                ).unwrap())
            } else {
                Ok(int_val)
            }
        } else {
            Err(CodegenError::InvalidArguments(
                format!("{} must be an integer", context)
            ))
        }
    }

    fn coerce_to_i64(&self, val: BasicValueEnum<'ctx>, context: &str) -> Result<inkwell::values::IntValue<'ctx>, CodegenError> {
        if val.is_int_value() {
            let int_val = val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                Ok(self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "coerce_i64"
                ).unwrap())
            } else {
                Ok(int_val)
            }
        } else {
            Err(CodegenError::InvalidArguments(
                format!("{} must be an integer", context)
            ))
        }
    }

    fn coerce_to_ptr(&self, val: BasicValueEnum<'ctx>, context: &str) -> Result<inkwell::values::PointerValue<'ctx>, CodegenError> {
        if val.is_pointer_value() {
            Ok(val.into_pointer_value())
        } else {
            Err(CodegenError::InvalidArguments(
                format!("{} must be a pointer", context)
            ))
        }
    }

    /// sys_socket(domain: i32, type: i32, protocol: i32) -> i32
    pub(crate) fn compile_sys_socket_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_socket")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_socket requires 3 arguments (domain, type, protocol)".to_string()
            ));
        }

        let socket_fn = self.module.get_function("socket")
            .ok_or_else(|| CodegenError::UndefinedFunction("socket".to_string()))?;

        let domain_val = self.compile_expr(&args[0])?;
        let type_val = self.compile_expr(&args[1])?;
        let protocol_val = self.compile_expr(&args[2])?;

        let domain = self.coerce_to_i32(domain_val, "sys_socket domain")?;
        let socket_type = self.coerce_to_i32(type_val, "sys_socket type")?;
        let protocol = self.coerce_to_i32(protocol_val, "sys_socket protocol")?;

        let call_site = self.builder
            .build_call(socket_fn, &[domain.into(), socket_type.into(), protocol.into()], "socket_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_bind(sockfd: i32, addr: *u8, addrlen: i32) -> i32
    pub(crate) fn compile_sys_bind_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_bind")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_bind requires 3 arguments (sockfd, addr, addrlen)".to_string()
            ));
        }

        let bind_fn = self.module.get_function("bind")
            .ok_or_else(|| CodegenError::UndefinedFunction("bind".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let addr_val = self.compile_expr(&args[1])?;
        let addrlen_val = self.compile_expr(&args[2])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_bind sockfd")?;
        let addr = self.coerce_to_ptr(addr_val, "sys_bind addr")?;
        let addrlen = self.coerce_to_i32(addrlen_val, "sys_bind addrlen")?;

        let call_site = self.builder
            .build_call(bind_fn, &[sockfd.into(), addr.into(), addrlen.into()], "bind_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_listen(sockfd: i32, backlog: i32) -> i32
    pub(crate) fn compile_sys_listen_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_listen")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_listen requires 2 arguments (sockfd, backlog)".to_string()
            ));
        }

        let listen_fn = self.module.get_function("listen")
            .ok_or_else(|| CodegenError::UndefinedFunction("listen".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let backlog_val = self.compile_expr(&args[1])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_listen sockfd")?;
        let backlog = self.coerce_to_i32(backlog_val, "sys_listen backlog")?;

        let call_site = self.builder
            .build_call(listen_fn, &[sockfd.into(), backlog.into()], "listen_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_accept(sockfd: i32, addr: *u8, addrlen: *i32) -> i32
    pub(crate) fn compile_sys_accept_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_accept")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_accept requires 3 arguments (sockfd, addr, addrlen)".to_string()
            ));
        }

        let accept_fn = self.module.get_function("accept")
            .ok_or_else(|| CodegenError::UndefinedFunction("accept".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let addr_val = self.compile_expr(&args[1])?;
        let addrlen_val = self.compile_expr(&args[2])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_accept sockfd")?;
        let addr = self.coerce_to_ptr(addr_val, "sys_accept addr")?;
        let addrlen = self.coerce_to_ptr(addrlen_val, "sys_accept addrlen")?;

        let call_site = self.builder
            .build_call(accept_fn, &[sockfd.into(), addr.into(), addrlen.into()], "accept_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_connect(sockfd: i32, addr: *u8, addrlen: i32) -> i32
    pub(crate) fn compile_sys_connect_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_connect")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_connect requires 3 arguments (sockfd, addr, addrlen)".to_string()
            ));
        }

        let connect_fn = self.module.get_function("connect")
            .ok_or_else(|| CodegenError::UndefinedFunction("connect".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let addr_val = self.compile_expr(&args[1])?;
        let addrlen_val = self.compile_expr(&args[2])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_connect sockfd")?;
        let addr = self.coerce_to_ptr(addr_val, "sys_connect addr")?;
        let addrlen = self.coerce_to_i32(addrlen_val, "sys_connect addrlen")?;

        let call_site = self.builder
            .build_call(connect_fn, &[sockfd.into(), addr.into(), addrlen.into()], "connect_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_send(sockfd: i32, buf: *u8, len: i64, flags: i32) -> i64
    pub(crate) fn compile_sys_send_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_send")?;

        if args.len() != 4 {
            return Err(CodegenError::InvalidArguments(
                "sys_send requires 4 arguments (sockfd, buf, len, flags)".to_string()
            ));
        }

        let send_fn = self.module.get_function("send")
            .ok_or_else(|| CodegenError::UndefinedFunction("send".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let buf_val = self.compile_expr(&args[1])?;
        let len_val = self.compile_expr(&args[2])?;
        let flags_val = self.compile_expr(&args[3])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_send sockfd")?;
        let buf = self.coerce_to_ptr(buf_val, "sys_send buf")?;
        let len = self.coerce_to_i64(len_val, "sys_send len")?;
        let flags = self.coerce_to_i32(flags_val, "sys_send flags")?;

        let call_site = self.builder
            .build_call(send_fn, &[sockfd.into(), buf.into(), len.into(), flags.into()], "send_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }

    /// sys_recv(sockfd: i32, buf: *u8, len: i64, flags: i32) -> i64
    pub(crate) fn compile_sys_recv_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_recv")?;

        if args.len() != 4 {
            return Err(CodegenError::InvalidArguments(
                "sys_recv requires 4 arguments (sockfd, buf, len, flags)".to_string()
            ));
        }

        let recv_fn = self.module.get_function("recv")
            .ok_or_else(|| CodegenError::UndefinedFunction("recv".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let buf_val = self.compile_expr(&args[1])?;
        let len_val = self.compile_expr(&args[2])?;
        let flags_val = self.compile_expr(&args[3])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_recv sockfd")?;
        let buf = self.coerce_to_ptr(buf_val, "sys_recv buf")?;
        let len = self.coerce_to_i64(len_val, "sys_recv len")?;
        let flags = self.coerce_to_i32(flags_val, "sys_recv flags")?;

        let call_site = self.builder
            .build_call(recv_fn, &[sockfd.into(), buf.into(), len.into(), flags.into()], "recv_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }

    /// sys_sendto(sockfd: i32, buf: *u8, len: i64, flags: i32, dest_addr: *u8, addrlen: i32) -> i64
    pub(crate) fn compile_sys_sendto_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_sendto")?;

        if args.len() != 6 {
            return Err(CodegenError::InvalidArguments(
                "sys_sendto requires 6 arguments (sockfd, buf, len, flags, dest_addr, addrlen)".to_string()
            ));
        }

        let sendto_fn = self.module.get_function("sendto")
            .ok_or_else(|| CodegenError::UndefinedFunction("sendto".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let buf_val = self.compile_expr(&args[1])?;
        let len_val = self.compile_expr(&args[2])?;
        let flags_val = self.compile_expr(&args[3])?;
        let dest_addr_val = self.compile_expr(&args[4])?;
        let addrlen_val = self.compile_expr(&args[5])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_sendto sockfd")?;
        let buf = self.coerce_to_ptr(buf_val, "sys_sendto buf")?;
        let len = self.coerce_to_i64(len_val, "sys_sendto len")?;
        let flags = self.coerce_to_i32(flags_val, "sys_sendto flags")?;
        let dest_addr = self.coerce_to_ptr(dest_addr_val, "sys_sendto dest_addr")?;
        let addrlen = self.coerce_to_i32(addrlen_val, "sys_sendto addrlen")?;

        let call_site = self.builder
            .build_call(sendto_fn, &[
                sockfd.into(),
                buf.into(),
                len.into(),
                flags.into(),
                dest_addr.into(),
                addrlen.into(),
            ], "sendto_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }

    /// sys_recvfrom(sockfd: i32, buf: *u8, len: i64, flags: i32, src_addr: *u8, addrlen: *i32) -> i64
    pub(crate) fn compile_sys_recvfrom_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_recvfrom")?;

        if args.len() != 6 {
            return Err(CodegenError::InvalidArguments(
                "sys_recvfrom requires 6 arguments (sockfd, buf, len, flags, src_addr, addrlen)".to_string()
            ));
        }

        let recvfrom_fn = self.module.get_function("recvfrom")
            .ok_or_else(|| CodegenError::UndefinedFunction("recvfrom".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let buf_val = self.compile_expr(&args[1])?;
        let len_val = self.compile_expr(&args[2])?;
        let flags_val = self.compile_expr(&args[3])?;
        let src_addr_val = self.compile_expr(&args[4])?;
        let addrlen_val = self.compile_expr(&args[5])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_recvfrom sockfd")?;
        let buf = self.coerce_to_ptr(buf_val, "sys_recvfrom buf")?;
        let len = self.coerce_to_i64(len_val, "sys_recvfrom len")?;
        let flags = self.coerce_to_i32(flags_val, "sys_recvfrom flags")?;
        let src_addr = self.coerce_to_ptr(src_addr_val, "sys_recvfrom src_addr")?;
        let addrlen = self.coerce_to_ptr(addrlen_val, "sys_recvfrom addrlen")?;

        let call_site = self.builder
            .build_call(recvfrom_fn, &[
                sockfd.into(),
                buf.into(),
                len.into(),
                flags.into(),
                src_addr.into(),
                addrlen.into(),
            ], "recvfrom_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }

    /// sys_setsockopt(sockfd: i32, level: i32, optname: i32, optval: *u8, optlen: i32) -> i32
    pub(crate) fn compile_sys_setsockopt_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_setsockopt")?;

        if args.len() != 5 {
            return Err(CodegenError::InvalidArguments(
                "sys_setsockopt requires 5 arguments (sockfd, level, optname, optval, optlen)".to_string()
            ));
        }

        let setsockopt_fn = self.module.get_function("setsockopt")
            .ok_or_else(|| CodegenError::UndefinedFunction("setsockopt".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let level_val = self.compile_expr(&args[1])?;
        let optname_val = self.compile_expr(&args[2])?;
        let optval_val = self.compile_expr(&args[3])?;
        let optlen_val = self.compile_expr(&args[4])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_setsockopt sockfd")?;
        let level = self.coerce_to_i32(level_val, "sys_setsockopt level")?;
        let optname = self.coerce_to_i32(optname_val, "sys_setsockopt optname")?;
        let optval = self.coerce_to_ptr(optval_val, "sys_setsockopt optval")?;
        let optlen = self.coerce_to_i32(optlen_val, "sys_setsockopt optlen")?;

        let call_site = self.builder
            .build_call(setsockopt_fn, &[
                sockfd.into(),
                level.into(),
                optname.into(),
                optval.into(),
                optlen.into(),
            ], "setsockopt_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_getsockopt(sockfd: i32, level: i32, optname: i32, optval: *u8, optlen: *i32) -> i32
    pub(crate) fn compile_sys_getsockopt_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_getsockopt")?;

        if args.len() != 5 {
            return Err(CodegenError::InvalidArguments(
                "sys_getsockopt requires 5 arguments (sockfd, level, optname, optval, optlen)".to_string()
            ));
        }

        let getsockopt_fn = self.module.get_function("getsockopt")
            .ok_or_else(|| CodegenError::UndefinedFunction("getsockopt".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let level_val = self.compile_expr(&args[1])?;
        let optname_val = self.compile_expr(&args[2])?;
        let optval_val = self.compile_expr(&args[3])?;
        let optlen_val = self.compile_expr(&args[4])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_getsockopt sockfd")?;
        let level = self.coerce_to_i32(level_val, "sys_getsockopt level")?;
        let optname = self.coerce_to_i32(optname_val, "sys_getsockopt optname")?;
        let optval = self.coerce_to_ptr(optval_val, "sys_getsockopt optval")?;
        let optlen = self.coerce_to_ptr(optlen_val, "sys_getsockopt optlen")?;

        let call_site = self.builder
            .build_call(getsockopt_fn, &[
                sockfd.into(),
                level.into(),
                optname.into(),
                optval.into(),
                optlen.into(),
            ], "getsockopt_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_shutdown(sockfd: i32, how: i32) -> i32
    pub(crate) fn compile_sys_shutdown_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_shutdown")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_shutdown requires 2 arguments (sockfd, how)".to_string()
            ));
        }

        let shutdown_fn = self.module.get_function("shutdown")
            .ok_or_else(|| CodegenError::UndefinedFunction("shutdown".to_string()))?;

        let sockfd_val = self.compile_expr(&args[0])?;
        let how_val = self.compile_expr(&args[1])?;

        let sockfd = self.coerce_to_i32(sockfd_val, "sys_shutdown sockfd")?;
        let how = self.coerce_to_i32(how_val, "sys_shutdown how")?;

        let call_site = self.builder
            .build_call(shutdown_fn, &[sockfd.into(), how.into()], "shutdown_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }
}
