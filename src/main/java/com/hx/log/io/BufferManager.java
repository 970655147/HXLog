/**
 * file name : BufferUtils.java
 * created at : 20:06:51 2016-12-30
 * created by 970655147
 */

package com.hx.log.io;

import com.hx.log.io.interf.BuffSizeEstimator;
import com.hx.log.io.interf.BufferHandler;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static com.hx.log.util.Tools.assert0;

public final class BufferManager {

    // ------------ ������� ------- 2016.03.16 -------------

    /**
     * Ĭ�ϵ�ˢ��buffer��ֵ
     */
    public static int DEFAULT_BUFF_THRESHOLD = 128 << 10;
    /**
     * Ĭ�ϵ�buffSizeEstimator
     */
    public static BuffSizeEstimator DEFAULT_BUFFSIZE_ESTIMATOR = new BuffSizeEstimator() {
        public int getBuffSize(int threshold) {
            return threshold + (threshold >> 3);
        }
    };
    /**
     * Ĭ�ϵ�buffHandler
     */
    public static BufferHandler DEFAULT_BUFF_HANDLER = new BufferHandler() {
        @Override
        public void beforeHandle(BuffInfo buffInfo) throws Exception {

        }

        @Override
        public void handleBuffer(BuffInfo buffInfo) throws Exception {
            flushBuffer(buffInfo.getSb(), buffInfo.getOutputPath(), buffInfo.getCharset());
        }

        @Override
        public void afterHandle(BuffInfo buffInfo) throws Exception {

        }
    };

    /**
     * buffName -> buffInfo
     */
    private Map<String, BuffInfo> bufferToBuffInfo = new HashMap<>();

    /**
     * ��ʼ��
     */
    public BufferManager() {

    }

    /**
     * ��ȡ���еĻ�������key�ļ���
     *
     * @return java.util.Set<java.lang.String>
     * @author Jerry.X.He
     * @date 5/5/2017 12:18 AM
     * @since 1.0
     */
    public Set<String> buffNames() {
        return new HashSet<>(bufferToBuffInfo.keySet());
    }

    /**
     * ����һ��������
     *
     * @param buffName          ������buffName
     * @param outputPath        ����������ļ���·��
     * @param charset           ������ַ���
     * @param threshold         �������ֵ
     * @param buffSizeEstimator buffSizeEstimate
     * @param handler           buffHandler
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:18 AM
     * @since 1.0
     */
    public void createAnBuffer(String buffName, String outputPath, String charset, int threshold,
                               BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
        assert0(buffName != null, "'buffName' can't be null ");
        assert0(outputPath != null, "'outputPath' can't be null ");
        assert0(charset != null, "'charset' can't be null ");
        assert0(threshold > 0, "'threshold' must > 0 ");
        assert0(buffSizeEstimator != null, "'buffSizeEstimator' can't be null ");
        assert0(handler != null, "'handler' can't be null ");

        if (bufExists(buffName)) {
            throw new RuntimeException("the buffInfo with key : " + buffName + " is already exists !");
        }

        BuffInfo buffInfo = new BuffInfo(outputPath, charset, threshold, buffSizeEstimator, handler);
        bufferToBuffInfo.put(buffName, buffInfo);
    }

    public void createAnBuffer(String buffName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
        createAnBuffer(buffName, outputPath, charset, threshold, buffSizeEstimator, DEFAULT_BUFF_HANDLER);
    }

    public void createAnBuffer(String buffName, String outputPath, String charset) {
        createAnBuffer(buffName, outputPath, charset, DEFAULT_BUFF_THRESHOLD, DEFAULT_BUFFSIZE_ESTIMATOR);
    }

    public void createAnBuffer(String buffName, String outputPath) {
        createAnBuffer(buffName, outputPath, Tools.DEFAULT_CHARSET);
    }

    /**
     * ���buffName��Ӧ�Ļ��岻����, �Ŵ���Buffer
     *
     * @param buffName          ������buffName
     * @param outputPath        ����������ļ���·��
     * @param charset           ������ַ���
     * @param threshold         �������ֵ
     * @param buffSizeEstimator buffSizeEstimate
     * @param handler           buffHandler
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset, int threshold,
                                          BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
        if (!bufExists(buffName)) {
            BuffInfo buffInfo = new BuffInfo(outputPath, charset, threshold, buffSizeEstimator, handler);
            bufferToBuffInfo.put(buffName, buffInfo);
        }
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
        createAnBufferIfNotExists(buffName, outputPath, charset, threshold, buffSizeEstimator, DEFAULT_BUFF_HANDLER);
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset, BufferHandler handler) {
        createAnBufferIfNotExists(buffName, outputPath, charset, DEFAULT_BUFF_THRESHOLD, DEFAULT_BUFFSIZE_ESTIMATOR, handler);
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath, String charset) {
        createAnBufferIfNotExists(buffName, outputPath, charset, DEFAULT_BUFF_HANDLER);
    }

    public void createAnBufferIfNotExists(String buffName, String outputPath) {
        createAnBufferIfNotExists(buffName, outputPath, Tools.DEFAULT_CHARSET);
    }

    /**
     * �رո�����Buffer
     *
     * @param buffName ������buffName
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public void closeAnBuffer(String buffName) throws Exception {
        flushBuffer(buffName, true);
    }

    /**
     * �ر����е�Buffer
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public void closeAllBuffer() throws Exception {
        for (String buffName : buffNames()) {
            closeAnBuffer(buffName);
        }
    }

    /**
     * �жϸ�����buffName��buffer�Ƿ����
     *
     * @param buffName ������buffName
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public boolean bufExists(String buffName) {
        return getBuffInfo(buffName) != null;
    }

    /**
     * ��ȡ��buffName��Ӧ��BuffInfo
     *
     * @param buffName ������buffName
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:20 AM
     * @since 1.0
     */
    public BuffInfo getBuffInfo(String buffName) {
        return bufferToBuffInfo.get(buffName);
    }

    /**
     * ������Ļ�������������� �����buffer�е������Ƿ񳬹�����ֵ
     *
     * @param buffName   ������buffName
     * @param content    ��Ҫ�����content
     * @param appendCRLF �Ƿ���Ҫ�س�
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:22 AM
     * @since 1.0
     */
    public void appendBuffer(String buffName, String content, boolean appendCRLF) throws Exception {
        assert0(buffName != null, "'buffName' can't be null ");
        if (!bufExists(buffName)) {
            throw new RuntimeException("have no buffInfo with key : " + buffName + ", please createAnBuffer first !");
        }

        BuffInfo buffInfo = bufferToBuffInfo.get(buffName);
        buffInfo.getSb().append(content);
        if (appendCRLF) {
            buffInfo.getSb().append(Tools.CRLF);
        }
        if (buffInfo.getSb().length() >= buffInfo.getThreshold()) {
            buffInfo.getHandler().beforeHandle(buffInfo);
            synchronized (buffInfo.getSb()) {
                if (buffInfo.getSb().length() >= buffInfo.getThreshold()) {
                    // judge if 'buf' exists in case of 'MultiThreadConcurrent'
                    if (bufExists(buffName)) {
//						flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset, logFlags);
                        buffInfo.getHandler().handleBuffer(buffInfo);
                    } else {
                        Log.log("the buffer : '" + buffName + "' already be removed !");
                    }
                }
            }
            buffInfo.getHandler().afterHandle(buffInfo);
        }
    }

    public void appendBuffer(String buffName, String content) throws Exception {
        appendBuffer(buffName, content, false);
    }

    public void appendBufferCRLF(String buffName, String content) throws Exception {
        appendBuffer(buffName, content, true);
    }

    /**
     * ˢ�����������
     *
     * @param buffName    ������buffName
     * @param isLastBatch �Ƿ�ˢ������֮��, �رջ���
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:22 AM
     * @since 1.0
     */
    public void flushBuffer(String buffName, boolean isLastBatch) throws Exception {
        assert0(buffName != null, "'buffName' can't be null ");
        if (!bufExists(buffName)) {
            throw new RuntimeException("have no buffInfo with key : " + buffName + ", please createAnBuffer first !");
        }

        BuffInfo buffInfo = bufferToBuffInfo.get(buffName);
        if (buffInfo.getSb().length() > 0) {
            buffInfo.getHandler().beforeHandle(buffInfo);
            synchronized (buffInfo.getSb()) {
                if (buffInfo.getSb().length() > 0) {
                    // judge if 'buf' exists in case of 'MultiThreadConcurrent'
                    if (bufExists(buffName)) {
//						flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset, logFlags);
                        buffInfo.getHandler().handleBuffer(buffInfo);

                        if (isLastBatch) {
                            bufferToBuffInfo.remove(buffName);
                        }
                    } else {
                        Log.log("the buffer : '" + buffName + "' already be removed !");
                    }
                }
            }
            buffInfo.getHandler().afterHandle(buffInfo);
        }
    }

    public void flushBuffer(String buffName) throws Exception {
        flushBuffer(buffName, false);
    }

    // update the step 'flushDataToPath' into 'threadPoolExecutor'		at 2016.04.16

    /**
     * ˢ��sb�е����ݵ��������ļ�
     *
     * @param sb      ��Ҫд��������
     * @param path    ��Ҫ������ļ�
     * @param charset �ֽ�ת��Ϊ�ַ��ı���
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:23 AM
     * @since 1.0
     */
    public static void flushBuffer(final StringBuffer sb, final String path, final String charset) throws IOException {
        assert0(sb != null, "'sb' can't be null ");
        assert0(path != null, "'path' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        // move 'nextThree' a head incase of 'buff.sb.length >= buff.threshold', got an circle, but can't clear 'buff.sb'		at 2016.04.23
        long kbLength = Tools.getKBytesByBytes(sb.length());
        String content = sb.toString();
        sb.setLength(0);

        if (!Tools.THREAD_POOL.isShutdown()) {
            Tools.append(content, path, charset, true);
        } else {
            Tools.append(content, path, charset, false);
        }
    }

    public void flushBuffer(StringBuffer sb, String path) throws IOException {
        flushBuffer(sb, path, Tools.DEFAULT_CHARSET);
    }


}
