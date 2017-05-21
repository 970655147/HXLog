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
     * ������ LogLevel ������
     */
    private String mode;
    /**
     * ������ LogLevel �����ȼ�
     */
    private int priority;

    /**
     * ��ʼ��
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
     * ��ȡ������mode��Ӧ�� LogLevel, ���û�ж�Ӧ��LogLevel, ���� LogLevel.OTHERS
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
     * ��ȡ��ǰ LogLevel ��Ӧ��mode
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
     * �жϵ�ǰ�� LogLevel �Ƿ������ڻ��ߵ��� ������LogLevel
     *
     * @param other ������LogLevel
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
     * �жϵ�ǰ�� LogLevel �Ƿ������� ������LogLevel
     *
     * @param other ������LogLevel
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
     * ��ֹ�ݹ������ĸ���������
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/15/2017 8:48 PM
     */
    private static class IdxHolder {
        /**
         * ���е����ģʽ
         */
        public static final String[] LOG_MODES = {"LOG", "INFO", "DEBUG", "WARNNING", "ERROR", "FATAL", "OTHERS"};
        /**
         * ���������Ĺ���
         */
        public static IdxIterator idxGenerator = new IdxGenerator();
        /**
         * �������һ��next() ���صĽ��
         */
        private static int now = -1;

        /**
         * ��ȡ��һ������
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
         * ��ȡ��һ��next() ���صĽ��
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
