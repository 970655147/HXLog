package com.hx.log.validator.validator;

import com.hx.attr_handler.attr_handler.StandardHandlerParser;
import com.hx.attr_handler.attr_handler.operation.interf.OperationAttrHandler;
import com.hx.attr_handler.util.AttrHandlerConstants;
import com.hx.attr_handler.util.AttrHandlerUtils;
import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * Jerry.X.He5/4/20179:08 PM AttrHandlerValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 8:39 PM
 */
public class AttrHandlerValidator implements Validator<String> {

    /**
     * 传入的handlerString构造的handler
     */
    private OperationAttrHandler handler;
    /**
     * 传入的handlerString
     */
    private String handlerStr;

    /**
     * 初始化
     *
     * @param handlerStr 给定的handlerString
     * @since 1.0
     */
    public AttrHandlerValidator(String handlerStr) {
        setHandlerStr(handlerStr);
    }

    public AttrHandlerValidator() {

    }

    /**
     * setter & getter
     */
    public String getHandlerStr() {
        return handlerStr;
    }

    public AttrHandlerValidator setHandlerStr(String handlerStr) {
        Tools.assert0(handlerStr != null, "'handlerStr' can't be null !");
        this.handler = AttrHandlerUtils.handlerParse(handlerStr, AttrHandlerConstants.HANDLER);
        Tools.assert0(this.handler.operationReturn() == StandardHandlerParser.Types.Boolean, "the handler's returnType must be Bool !");
        this.handlerStr = handlerStr;
        return this;
    }

    @Override
    public Result validate(String obj, Object extra) {
        if ((obj == null) || (handler == null) ) {
            return failed(obj);
        }

        boolean matches = Boolean.valueOf(handler.handle(obj));
        if (!matches) {
            return failed(obj);
        }

        return ValidateResultUtils.success();
    }

    /**
     * 比较失败之后返回的结果
     *
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private Result failed(String obj) {
        return ValidateResultUtils.failed("the String : '" + obj + "' does not math handlerString : '" + handlerStr + "' !");
    }

}
