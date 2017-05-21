package com.hx.log.log;

import com.hx.common.interf.idx.IdxIterator;
import com.hx.log.idx.IdxGenerator;

/**
 * LogLevel
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/15/2017 8:40 PM
 */
public enum LogLevel {
    /**
     * LogLevel.LOG
     */
    LOG(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current()),
    /**
     * LogLevel.INFO
     */
    INFO(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current()),
    /**
     * LogLevel.DEBUG
     */
    DEBUG(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current()),
    /**
     * LogLevel.WARN
     */
    WARN(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current()),
    /**
     * LogLevel.ERROR
     */
    ERR(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current()),
    /**
     * LogLevel.FATAL
     */
    FATAL(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current()),
    /**
     * LogLevel.OTHERS
     */
    OTHERS(IdxHolder.LOG_MODES[IdxHolder.next()], IdxHolder.current());


    /**
     * 给定的 LogLevel 的名称
     */
    private String mode;
    /**
     * 给定的 LogLevel 的优先级
     */
    private int priority;

    /**
     * 初始化
     *
     * @param mode     mode
     * @param priority priority
     * @since 1.0
     */
    LogLevel(String mode, int priority) {
        this.mode = mode;
        this.priority = priority;
    }

    /**
     * 获取给定的mode对应的 LogLevel, 如果没有对应的LogLevel, 返回 LogLevel.OTHERS
     *
     * @param mode mode
     * @return com.hx.log.log.LogLevel
     * @author Jerry.X.He
     * @date 5/15/2017 8:52 PM
     * @since 1.0
     */
    public static LogLevel of(String mode) {
        for (LogLevel level : values()) {
            if (level.mode.equalsIgnoreCase(mode)) {
                return level;
            }
        }

        return OTHERS;
    }

    /**
     * 获取当前 LogLevel 对应的mode
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/17/2017 8:43 PM
     * @since 1.0
     */
    public String mode() {
        return mode;
    }

    /**
     * 判断当前的 LogLevel 是否优先于或者等于 给定的LogLevel
     *
     * @param other 给定的LogLevel
     * @return boolean
     * @author Jerry.X.He
     * @date 5/15/2017 9:02 PM
     * @since 1.0
     */
    public boolean gte(LogLevel other) {
        if (other == null) {
            return true;
        }

        return this.priority - other.priority >= 0;
    }

    /**
     * 判断当前的 LogLevel 是否优先于 给定的LogLevel
     *
     * @param other 给定的LogLevel
     * @return boolean
     * @author Jerry.X.He
     * @date 5/15/2017 9:02 PM
     * @since 1.0
     */
    public boolean gt(LogLevel other) {
        if (other == null) {
            return true;
        }

        return this.priority - other.priority > 0;
    }

    /**
     * 防止递归依赖的辅助工具类
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/15/2017 8:48 PM
     */
    private static class IdxHolder {
        /**
         * 所有的输出模式
         */
        public static final String[] LOG_MODES = {"LOG", "INFO", "DEBUG", "WARNNING", "ERROR", "FATAL", "OTHERS"};
        /**
         * 生成索引的工具
         */
        public static IdxIterator idxGenerator = new IdxGenerator();
        /**
         * 缓存的上一个next() 返回的结果
         */
        private static int now = -1;

        /**
         * 获取下一个索引
         *
         * @return int
         * @author Jerry.X.He
         * @date 5/15/2017 8:49 PM
         * @since 1.0
         */
        public static int next() {
            now = idxGenerator.next();
            return now;
        }

        /**
         * 获取上一个next() 返回的结果
         *
         * @return int
         * @author Jerry.X.He
         * @date 5/15/2017 8:49 PM
         * @since 1.0
         */
        public static int current() {
            return now;
        }
    }
}
