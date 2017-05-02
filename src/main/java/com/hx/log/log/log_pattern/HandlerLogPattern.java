package com.hx.log.log.log_pattern;

import com.hx.attr_handler.attr_handler.operation.interf.OperationAttrHandler;
import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.log.LogPatternType;
import com.hx.log.log.log_pattern.interf.OneStringVariableLogPattern;
import com.hx.log.util.Constants;

/**
 * AttrHandler ´¦ÀíµÄLogPattern
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:22 PM
 */
public class HandlerLogPattern extends OneStringVariableLogPattern {

    private OperationAttrHandler attrHandler;

    public HandlerLogPattern(OperationAttrHandler attrHandler, String arg) {
        super(arg);
        this.attrHandler = attrHandler;
    }

    @Override
    public String pattern() {
        attrHandler.cleanImmediateReturnFlag();
        String res = attrHandler.handle(arg);
        if(attrHandler.immediateReturn() ) {
            attrHandler.handleImmediateReturn();
            res = Constants.EMPTY_STR;
        }

        return res;
    }

    @Override
    public LogPatternType type() {
        return LogPatternType.HANDLER;
    }

    @Override
    public LogPattern copyOf() {
        return this;
    }

}
